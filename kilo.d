module kilo;
@nogc nothrow:
extern (C):
__gshared:
/*** includes ***/

version = _DEFAULT_SOURCE;
version = _BSD_SOURCE;
version = _GNU_SOURCE;

public import core.stdc.ctype;
public import core.stdc.errno;
public import core.sys.posix.fcntl;
public import core.stdc.stdio;
public import core.stdc.stdarg;
public import core.stdc.stdlib;
public import core.stdc.string;
public import core.sys.posix.sys.ioctl;
public import core.sys.posix.sys.types;
public import core.sys.posix.termios;
public import core.stdc.time;
public import core.sys.posix.unistd;

/*** defines ***/

enum KILO_VERSION = "0.0.1";
enum KILO_TAB_STOP = 8;
enum KILO_QUIT_TIMES = 3;

enum string CTRL_KEY(string k) = ` ((k) & 0x1f)`;

enum editorKey {
  BACKSPACE = 127,
  ARROW_LEFT = 1000,
  ARROW_RIGHT,
  ARROW_UP,
  ARROW_DOWN,
  DEL_KEY,
  HOME_KEY,
  END_KEY,
  PAGE_UP,
  PAGE_DOWN
}

alias BACKSPACE = editorKey.BACKSPACE;
alias ARROW_LEFT = editorKey.ARROW_LEFT;
alias ARROW_RIGHT = editorKey.ARROW_RIGHT;
alias ARROW_UP = editorKey.ARROW_UP;
alias ARROW_DOWN = editorKey.ARROW_DOWN;
alias DEL_KEY = editorKey.DEL_KEY;
alias HOME_KEY = editorKey.HOME_KEY;
alias END_KEY = editorKey.END_KEY;
alias PAGE_UP = editorKey.PAGE_UP;
alias PAGE_DOWN = editorKey.PAGE_DOWN;

enum editorHighlight {
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH
}

alias HL_NORMAL = editorHighlight.HL_NORMAL;
alias HL_COMMENT = editorHighlight.HL_COMMENT;
alias HL_MLCOMMENT = editorHighlight.HL_MLCOMMENT;
alias HL_KEYWORD1 = editorHighlight.HL_KEYWORD1;
alias HL_KEYWORD2 = editorHighlight.HL_KEYWORD2;
alias HL_STRING = editorHighlight.HL_STRING;
alias HL_NUMBER = editorHighlight.HL_NUMBER;
alias HL_MATCH = editorHighlight.HL_MATCH;

enum HL_HIGHLIGHT_NUMBERS = (1 << 0);
enum HL_HIGHLIGHT_STRINGS = (1 << 1);

/*** data ***/

struct editorSyntax {
  char* filetype;
  char** filematch;
  char** keywords;
  char* singleline_comment_start;
  char* multiline_comment_start;
  char* multiline_comment_end;
  int flags;
}

struct erow {
  int idx;
  int size;
  int rsize;
  char* chars;
  char* render;
  ubyte* hl;
  int hl_open_comment;
}

struct editorConfig {
  int cx, cy;
  int rx;
  int rowoff;
  int coloff;
  int screenrows;
  int screencols;
  int numrows;
  erow* row;
  int dirty;
  char* filename;
  char[80] statusmsg = 0;
  time_t statusmsg_time;
  editorSyntax* syntax;
  termios orig_termios;
}

editorConfig E;

/*** filetypes ***/

char*[4] C_HL_extensions = [".c", ".h", ".cpp", null];
char*[24] C_HL_keywords = [
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class", "case",

  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
  "void|", null
];

editorSyntax[2] HLDB = [
  [
    "c",
    C_HL_extensions,
    C_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  ],
];

enum HLDB_ENTRIES = (sizeof(HLDB) / sizeof(HLDB[0]));

/*** prototypes ***/

void editorSetStatusMessage(const(char)* fmt, ...);
void editorRefreshScreen();
char* editorPrompt(char* prompt, void function(char*, int) callback);

/*** terminal ***/

void die(const(char)* s) {
  write(STDOUT_FILENO, "\x1b[2J", 4);
  write(STDOUT_FILENO, "\x1b[H", 3);

  perror(s);
  exit(1);
}

void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("tcsetattr");
}

void enableRawMode() {
  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
    die("tcgetattr");
  atexit(&disableRawMode);

  termios raw = E.orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
    die("tcsetattr");
}

int editorReadKey() {
  int nread = void;
  char c = void;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN)
      die("read");
  }

  if (c == '\x1b') {
    char[3] seq = void;

    if (read(STDIN_FILENO, &seq[0], 1) != 1)
      return '\x1b';
    if (read(STDIN_FILENO, &seq[1], 1) != 1)
      return '\x1b';

    if (seq[0] == '[') {
      if (seq[1] >= '0' && seq[1] <= '9') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1)
          return '\x1b';
        if (seq[2] == '~') {
          switch (seq[1]) {
          case '1':
            return HOME_KEY;
          case '3':
            return DEL_KEY;
          case '4':
            return END_KEY;
          case '5':
            return PAGE_UP;
          case '6':
            return PAGE_DOWN;
          case '7':
            return HOME_KEY;
          case '8':
            return END_KEY;
          default:
            break;
          }
        }
      }
      else {
        switch (seq[1]) {
        case 'A':
          return ARROW_UP;
        case 'B':
          return ARROW_DOWN;
        case 'C':
          return ARROW_RIGHT;
        case 'D':
          return ARROW_LEFT;
        case 'H':
          return HOME_KEY;
        case 'F':
          return END_KEY;
        default:
          break;
        }
      }
    }
    else if (seq[0] == 'O') {
      switch (seq[1]) {
      case 'H':
        return HOME_KEY;
      case 'F':
        return END_KEY;
      default:
        break;
      }
    }

    return '\x1b';
  }
  else {
    return c;
  }
}

int getCursorPosition(int* rows, int* cols) {
  char[32] buf = void;
  uint i = 0;

  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4)
    return -1;

  while (i < sizeof(buf).ptr - 1) {
    if (read(STDIN_FILENO, &buf[i], 1) != 1)
      break;
    if (buf[i] == 'R')
      break;
    i++;
  }
  buf[i] = '\0';

  if (buf[0] != '\x1b' || buf[1] != '[')
    return -1;
  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2)
    return -1;

  return 0;
}

int getWindowSize(int* rows, int* cols) {
  winsize ws = void;

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12)
      return -1;
    return getCursorPosition(rows, cols);
  }
  else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

/*** syntax highlighting ***/

int is_separator(int c) {
  return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != null;
}

void editorUpdateSyntax(erow* row) {
  row.hl = realloc(row.hl, row.rsize);
  memset(row.hl, HL_NORMAL, row.rsize);

  if (E.syntax == null)
    return;

  char** keywords = E.syntax.keywords;

  char* scs = E.syntax.singleline_comment_start;
  char* mcs = E.syntax.multiline_comment_start;
  char* mce = E.syntax.multiline_comment_end;

  int scs_len = scs ? strlen(scs) : 0;
  int mcs_len = mcs ? strlen(mcs) : 0;
  int mce_len = mce ? strlen(mce) : 0;

  int prev_sep = 1;
  int in_string = 0;
  int in_comment = (row.idx > 0 && E.row[row.idx - 1].hl_open_comment);

  int i = 0;
  while (i < row.rsize) {
    char c = row.render[i];
    ubyte prev_hl = (i > 0) ? row.hl[i - 1] : HL_NORMAL;

    if (scs_len && !in_string && !in_comment) {
      if (!strncmp(&row.render[i], scs, scs_len)) {
        memset(&row.hl[i], HL_COMMENT, row.rsize - i);
        break;
      }
    }

    if (mcs_len && mce_len && !in_string) {
      if (in_comment) {
        row.hl[i] = HL_MLCOMMENT;
        if (!strncmp(&row.render[i], mce, mce_len)) {
          memset(&row.hl[i], HL_MLCOMMENT, mce_len);
          i += mce_len;
          in_comment = 0;
          prev_sep = 1;
          continue;
        }
        else {
          i++;
          continue;
        }
      }
      else if (!strncmp(&row.render[i], mcs, mcs_len)) {
        memset(&row.hl[i], HL_MLCOMMENT, mcs_len);
        i += mcs_len;
        in_comment = 1;
        continue;
      }
    }

    if (E.syntax.flags & HL_HIGHLIGHT_STRINGS) {
      if (in_string) {
        row.hl[i] = HL_STRING;
        if (c == '\\' && i + 1 < row.rsize) {
          row.hl[i + 1] = HL_STRING;
          i += 2;
          continue;
        }
        if (c == in_string)
          in_string = 0;
        i++;
        prev_sep = 1;
        continue;
      }
      else {
        if (c == '"' || c == '\'') {
          in_string = c;
          row.hl[i] = HL_STRING;
          i++;
          continue;
        }
      }
    }

    if (E.syntax.flags & HL_HIGHLIGHT_NUMBERS) {
      if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
        (c == '.' && prev_hl == HL_NUMBER)) {
        row.hl[i] = HL_NUMBER;
        i++;
        prev_sep = 0;
        continue;
      }
    }

    if (prev_sep) {
      int j = void;
      for (j = 0; keywords[j]; j++) {
        int klen = strlen(keywords[j]);
        int kw2 = keywords[j][klen - 1] == '|';
        if (kw2)
          klen--;

        if (!strncmp(&row.render[i], keywords[j], klen) &&
          is_separator(row.render[i + klen])) {
          memset(&row.hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
          i += klen;
          break;
        }
      }
      if (keywords[j] != null) {
        prev_sep = 0;
        continue;
      }
    }

    prev_sep = is_separator(c);
    i++;
  }

  int changed = (row.hl_open_comment != in_comment);
  row.hl_open_comment = in_comment;
  if (changed && row.idx + 1 < E.numrows)
    editorUpdateSyntax(&E.row[row.idx + 1]);
}

int editorSyntaxToColor(int hl) {
  switch (hl) {
  case HL_COMMENT:
  case HL_MLCOMMENT:
    return 36;
  case HL_KEYWORD1:
    return 33;
  case HL_KEYWORD2:
    return 32;
  case HL_STRING:
    return 35;
  case HL_NUMBER:
    return 31;
  case HL_MATCH:
    return 34;
  default:
    return 37;
  }
}

void editorSelectSyntaxHighlight() {
  E.syntax = null;
  if (E.filename == null)
    return;

  char* ext = strrchr(E.filename, '.');

  for (uint j = 0; j < HLDB_ENTRIES; j++) {
    editorSyntax* s = &HLDB[j];
    uint i = 0;
    while (s.filematch[i]) {
      int is_ext = (s.filematch[i][0] == '.');
      if ((is_ext && ext && !strcmp(ext, s.filematch[i])) ||
        (!is_ext && strstr(E.filename, s.filematch[i]))) {
        E.syntax = s;

        int filerow = void;
        for (filerow = 0; filerow < E.numrows; filerow++) {
          editorUpdateSyntax(&E.row[filerow]);
        }

        return;
      }
      i++;
    }
  }
}

/*** row operations ***/

int editorRowCxToRx(erow* row, int cx) {
  int rx = 0;
  int j = void;
  for (j = 0; j < cx; j++) {
    if (row.chars[j] == '\t')
      rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
    rx++;
  }
  return rx;
}

int editorRowRxToCx(erow* row, int rx) {
  int cur_rx = 0;
  int cx = void;
  for (cx = 0; cx < row.size; cx++) {
    if (row.chars[cx] == '\t')
      cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
    cur_rx++;

    if (cur_rx > rx)
      return cx;
  }
  return cx;
}

void editorUpdateRow(erow* row) {
  int tabs = 0;
  int j = void;
  for (j = 0; j < row.size; j++)
    if (row.chars[j] == '\t')
      tabs++;

  free(row.render);
  row.render = malloc(row.size + tabs * (KILO_TAB_STOP - 1) + 1);

  int idx = 0;
  for (j = 0; j < row.size; j++) {
    if (row.chars[j] == '\t') {
      row.render[idx++] = ' ';
      while (idx % KILO_TAB_STOP != 0)
        row.render[idx++] = ' ';
    }
    else {
      row.render[idx++] = row.chars[j];
    }
  }
  row.render[idx] = '\0';
  row.rsize = idx;

  editorUpdateSyntax(row);
}

void editorInsertRow(int at, char* s, size_t len) {
  if (at < 0 || at > E.numrows)
    return;

  E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
  memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
  for (int j = at + 1; j <= E.numrows; j++)
    E.row[j].idx++;

  E.row[at].idx = at;

  E.row[at].size = len;
  E.row[at].chars = malloc(len + 1);
  memcpy(E.row[at].chars, s, len);
  E.row[at].chars[len] = '\0';

  E.row[at].rsize = 0;
  E.row[at].render = null;
  E.row[at].hl = null;
  E.row[at].hl_open_comment = 0;
  editorUpdateRow(&E.row[at]);

  E.numrows++;
  E.dirty++;
}

void editorFreeRow(erow* row) {
  free(row.render);
  free(row.chars);
  free(row.hl);
}

void editorDelRow(int at) {
  if (at < 0 || at >= E.numrows)
    return;
  editorFreeRow(&E.row[at]);
  memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
  for (int j = at; j < E.numrows - 1; j++)
    E.row[j].idx--;
  E.numrows--;
  E.dirty++;
}

void editorRowInsertChar(erow* row, int at, int c) {
  if (at < 0 || at > row.size)
    at = row.size;
  row.chars = realloc(row.chars, row.size + 2);
  memmove(&row.chars[at + 1], &row.chars[at], row.size - at + 1);
  row.size++;
  row.chars[at] = c;
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowAppendString(erow* row, char* s, size_t len) {
  row.chars = realloc(row.chars, row.size + len + 1);
  memcpy(&row.chars[row.size], s, len);
  row.size += len;
  row.chars[row.size] = '\0';
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowDelChar(erow* row, int at) {
  if (at < 0 || at >= row.size)
    return;
  memmove(&row.chars[at], &row.chars[at + 1], row.size - at);
  row.size--;
  editorUpdateRow(row);
  E.dirty++;
}

/*** editor operations ***/

void editorInsertChar(int c) {
  if (E.cy == E.numrows) {
    editorInsertRow(E.numrows, "", 0);
  }
  editorRowInsertChar(&E.row[E.cy], E.cx, c);
  E.cx++;
}

void editorInsertNewline() {
  if (E.cx == 0) {
    editorInsertRow(E.cy, "", 0);
  }
  else {
    erow* row = &E.row[E.cy];
    editorInsertRow(E.cy + 1, &row.chars[E.cx], row.size - E.cx);
    row = &E.row[E.cy];
    row.size = E.cx;
    row.chars[row.size] = '\0';
    editorUpdateRow(row);
  }
  E.cy++;
  E.cx = 0;
}

void editorDelChar() {
  if (E.cy == E.numrows)
    return;
  if (E.cx == 0 && E.cy == 0)
    return;

  erow* row = &E.row[E.cy];
  if (E.cx > 0) {
    editorRowDelChar(row, E.cx - 1);
    E.cx--;
  }
  else {
    E.cx = E.row[E.cy - 1].size;
    editorRowAppendString(&E.row[E.cy - 1], row.chars, row.size);
    editorDelRow(E.cy);
    E.cy--;
  }
}

/*** file i/o ***/

char* editorRowsToString(int* buflen) {
  int totlen = 0;
  int j = void;
  for (j = 0; j < E.numrows; j++)
    totlen += E.row[j].size + 1;
  *buflen = totlen;

  char* buf = malloc(totlen);
  char* p = buf;
  for (j = 0; j < E.numrows; j++) {
    memcpy(p, E.row[j].chars, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }

  return buf;
}

void editorOpen(char* filename) {
  free(E.filename);
  E.filename = strdup(filename);

  editorSelectSyntaxHighlight();

  FILE* fp = fopen(filename, "r");
  if (!fp)
    die("fopen");

  char* line = null;
  size_t linecap = 0;
  ssize_t linelen = void;
  while ((linelen = getline(&line, &linecap, fp)) != -1) {
    while (linelen > 0 && (line[linelen - 1] == '\n' ||
        line[linelen - 1] == '\r'))
      linelen--;
    editorInsertRow(E.numrows, line, linelen);
  }
  free(line);
  fclose(fp);
  E.dirty = 0;
}

void editorSave() {
  if (E.filename == null) {
    E.filename = editorPrompt("Save as: %s (ESC to cancel)", null);
    if (E.filename == null) {
      editorSetStatusMessage("Save aborted");
      return;
    }
    editorSelectSyntaxHighlight();
  }

  int len = void;
  char* buf = editorRowsToString(&len);

  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
  if (fd != -1) {
    if (ftruncate(fd, len) != -1) {
      if (write(fd, buf, len) == len) {
        close(fd);
        free(buf);
        E.dirty = 0;
        editorSetStatusMessage("%d bytes written to disk", len);
        return;
      }
    }
    close(fd);
  }

  free(buf);
  editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/*** find ***/

void editorFindCallback(char* query, int key) {
  static int last_match = -1;
  static int direction = 1;

  static int saved_hl_line;
  static char* saved_hl = null;

  if (saved_hl) {
    memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
    free(saved_hl);
    saved_hl = null;
  }

  if (key == '\r' || key == '\x1b') {
    last_match = -1;
    direction = 1;
    return;
  }
  else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
    direction = 1;
  }
  else if (key == ARROW_LEFT || key == ARROW_UP) {
    direction = -1;
  }
  else {
    last_match = -1;
    direction = 1;
  }

  if (last_match == -1)
    direction = 1;
  int current = last_match;
  int i = void;
  for (i = 0; i < E.numrows; i++) {
    current += direction;
    if (current == -1)
      current = E.numrows - 1;
    else if (current == E.numrows)
      current = 0;

    erow* row = &E.row[current];
    char* match = strstr(row.render, query);
    if (match) {
      last_match = current;
      E.cy = current;
      E.cx = editorRowRxToCx(row, match - row.render);
      E.rowoff = E.numrows;

      saved_hl_line = current;
      saved_hl = malloc(row.rsize);
      memcpy(saved_hl, row.hl, row.rsize);
      memset(&row.hl[match - row.render], HL_MATCH, strlen(query));
      break;
    }
  }
}

void editorFind() {
  int saved_cx = E.cx;
  int saved_cy = E.cy;
  int saved_coloff = E.coloff;
  int saved_rowoff = E.rowoff;

  char* query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)",
    &editorFindCallback);

  if (query) {
    free(query);
  }
  else {
    E.cx = saved_cx;
    E.cy = saved_cy;
    E.coloff = saved_coloff;
    E.rowoff = saved_rowoff;
  }
}

/*** append buffer ***/

struct abuf {
  char* b;
  int len;
}

enum ABUF_INIT = {NULL, 0};

void abAppend(abuf* ab, const(char)* s, int len) {
  char* new_ = realloc(ab.b, ab.len + len);

  if (new_ == null)
    return;
  memcpy(&new_[ab.len], s, len);
  ab.b = new_;
  ab.len += len;
}

void abFree(abuf* ab) {
  free(ab.b);
}

/*** output ***/

void editorScroll() {
  E.rx = 0;
  if (E.cy < E.numrows) {
    E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
  }

  if (E.cy < E.rowoff) {
    E.rowoff = E.cy;
  }
  if (E.cy >= E.rowoff + E.screenrows) {
    E.rowoff = E.cy - E.screenrows + 1;
  }
  if (E.rx < E.coloff) {
    E.coloff = E.rx;
  }
  if (E.rx >= E.coloff + E.screencols) {
    E.coloff = E.rx - E.screencols + 1;
  }
}

void editorDrawRows(abuf* ab) {
  int y = void;
  for (y = 0; y < E.screenrows; y++) {
    int filerow = y + E.rowoff;
    if (filerow >= E.numrows) {
      if (E.numrows == 0 && y == E.screenrows / 3) {
        char[80] welcome = void;
        int welcomelen = snprintf(welcome.ptr, welcome.sizeof,
          "Kilo editor -- version %s", KILO_VERSION);
        if (welcomelen > E.screencols)
          welcomelen = E.screencols;
        int padding = (E.screencols - welcomelen) / 2;
        if (padding) {
          abAppend(ab, "~", 1);
          padding--;
        }
        while (padding--)
          abAppend(ab, " ", 1);
        abAppend(ab, welcome.ptr, welcomelen);
      }
      else {
        abAppend(ab, "~", 1);
      }
    }
    else {
      int len = E.row[filerow].rsize - E.coloff;
      if (len < 0)
        len = 0;
      if (len > E.screencols)
        len = E.screencols;
      char* c = &E.row[filerow].render[E.coloff];
      ubyte* hl = &E.row[filerow].hl[E.coloff];
      int current_color = -1;
      int j = void;
      for (j = 0; j < len; j++) {
        if (iscntrl(c[j])) {
          char sym = (c[j] <= 26) ? '@' + c[j] : '?';
          abAppend(ab, "\x1b[7m", 4);
          abAppend(ab, &sym, 1);
          abAppend(ab, "\x1b[m", 3);
          if (current_color != -1) {
            char[16] buf = void;
            int clen = snprintf(buf.ptr, buf.sizeof, "\x1b[%dm", current_color);
            abAppend(ab, buf.ptr, clen);
          }
        }
        else if (hl[j] == HL_NORMAL) {
          if (current_color != -1) {
            abAppend(ab, "\x1b[39m", 5);
            current_color = -1;
          }
          abAppend(ab, &c[j], 1);
        }
        else {
          int color = editorSyntaxToColor(hl[j]);
          if (color != current_color) {
            current_color = color;
            char[16] buf = void;
            int clen = snprintf(buf.ptr, buf.sizeof, "\x1b[%dm", color);
            abAppend(ab, buf.ptr, clen);
          }
          abAppend(ab, &c[j], 1);
        }
      }
      abAppend(ab, "\x1b[39m", 5);
    }

    abAppend(ab, "\x1b[K", 3);
    abAppend(ab, "\r\n", 2);
  }
}

void editorDrawStatusBar(abuf* ab) {
  abAppend(ab, "\x1b[7m", 4);
  char[80] status = void, rstatus = void;
  int len = snprintf(status.ptr, status.sizeof, "%.20s - %d lines %s",
    E.filename ? E.filename : "[No Name]", E.numrows,
    E.dirty ? "(modified)" : "");
  int rlen = snprintf(rstatus.ptr, rstatus.sizeof, "%s | %d/%d",
    E.syntax ? E.syntax.filetype : "no ft", E.cy + 1, E.numrows);
  if (len > E.screencols)
    len = E.screencols;
  abAppend(ab, status.ptr, len);
  while (len < E.screencols) {
    if (E.screencols - len == rlen) {
      abAppend(ab, rstatus.ptr, rlen);
      break;
    }
    else {
      abAppend(ab, " ", 1);
      len++;
    }
  }
  abAppend(ab, "\x1b[m", 3);
  abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(abuf* ab) {
  abAppend(ab, "\x1b[K", 3);
  int msglen = strlen(E.statusmsg);
  if (msglen > E.screencols)
    msglen = E.screencols;
  if (msglen && time(null) - E.statusmsg_time < 5)
    abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen() {
  editorScroll();

  abuf ab = ABUF_INIT;

  abAppend(&ab, "\x1b[?25l", 6);
  abAppend(&ab, "\x1b[H", 3);

  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);

  char[32] buf = void;
  snprintf(buf.ptr, buf.sizeof, "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
    (E.rx - E.coloff) + 1);
  abAppend(&ab, buf.ptr, strlen(buf.ptr));

  abAppend(&ab, "\x1b[?25h", 6);

  write(STDOUT_FILENO, ab.b, ab.len);
  abFree(&ab);
}

void editorSetStatusMessage(const(char)* fmt, ...) {
  va_list ap = void;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, typeof(E.statusmsg).sizeof, fmt, ap);
  va_end(ap);
  E.statusmsg_time = time(null);
}

/*** input ***/

char* editorPrompt(char* prompt, void function(char*, int) callback) {
  size_t bufsize = 128;
  char* buf = malloc(bufsize);

  size_t buflen = 0;
  buf[0] = '\0';

  while (1) {
    editorSetStatusMessage(prompt, buf);
    editorRefreshScreen();

    int c = editorReadKey();
    if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
      if (buflen != 0)
        buf[--buflen] = '\0';
    }
    else if (c == '\x1b') {
      editorSetStatusMessage("");
      if (callback)
        callback(buf, c);
      free(buf);
      return null;
    }
    else if (c == '\r') {
      if (buflen != 0) {
        editorSetStatusMessage("");
        if (callback)
          callback(buf, c);
        return buf;
      }
    }
    else if (!iscntrl(c) && c < 128) {
      if (buflen == bufsize - 1) {
        bufsize *= 2;
        buf = realloc(buf, bufsize);
      }
      buf[buflen++] = c;
      buf[buflen] = '\0';
    }

    if (callback)
      callback(buf, c);
  }
}

void editorMoveCursor(int key) {
  erow* row = (E.cy >= E.numrows) ? null : &E.row[E.cy];

  switch (key) {
  case ARROW_LEFT:
    if (E.cx != 0) {
      E.cx--;
    }
    else if (E.cy > 0) {
      E.cy--;
      E.cx = E.row[E.cy].size;
    }
    break;
  case ARROW_RIGHT:
    if (row && E.cx < row.size) {
      E.cx++;
    }
    else if (row && E.cx == row.size) {
      E.cy++;
      E.cx = 0;
    }
    break;
  case ARROW_UP:
    if (E.cy != 0) {
      E.cy--;
    }
    break;
  case ARROW_DOWN:
    if (E.cy < E.numrows) {
      E.cy++;
    }
    break;
  default:
    break;
  }

  row = (E.cy >= E.numrows) ? null : &E.row[E.cy];
  int rowlen = row ? row.size : 0;
  if (E.cx > rowlen) {
    E.cx = rowlen;
  }
}

void editorProcessKeypress() {
  static int quit_times = KILO_QUIT_TIMES;

  int c = editorReadKey();

  switch (c) {
  case '\r':
    editorInsertNewline();
    break;

  case CTRL_KEY('q'):
    if (E.dirty && quit_times > 0) {
      editorSetStatusMessage("WARNING!!! File has unsaved changes. "
          ~ "Press Ctrl-Q %d more times to quit.", quit_times);
      quit_times--;
      return;
    }
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);
    exit(0);
    break;

  case CTRL_KEY('s'):
    editorSave();
    break;

  case HOME_KEY:
    E.cx = 0;
    break;

  case END_KEY:
    if (E.cy < E.numrows)
      E.cx = E.row[E.cy].size;
    break;

  case CTRL_KEY('f'):
    editorFind();
    break;

  case BACKSPACE:
  case CTRL_KEY('h'):
  case DEL_KEY:
    if (c == DEL_KEY)
      editorMoveCursor(ARROW_RIGHT);
    editorDelChar();
    break;

  case PAGE_UP:
  case PAGE_DOWN: {
      if (c == PAGE_UP) {
        E.cy = E.rowoff;
      }
      else if (c == PAGE_DOWN) {
        E.cy = E.rowoff + E.screenrows - 1;
        if (E.cy > E.numrows)
          E.cy = E.numrows;
      }

      int times = E.screenrows;
      while (times--)
        editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
    }
    break;

  case ARROW_UP:
  case ARROW_DOWN:
  case ARROW_LEFT:
  case ARROW_RIGHT:
    editorMoveCursor(c);
    break;

  case CTRL_KEY('l'):
  case '\x1b':
    break;

  default:
    editorInsertChar(c);
    break;
  }

  quit_times = KILO_QUIT_TIMES;
}

/*** init ***/

void initEditor() {
  E.cx = 0;
  E.cy = 0;
  E.rx = 0;
  E.rowoff = 0;
  E.coloff = 0;
  E.numrows = 0;
  E.row = null;
  E.dirty = 0;
  E.filename = null;
  E.statusmsg[0] = '\0';
  E.statusmsg_time = 0;
  E.syntax = null;

  if (getWindowSize(&E.screenrows, &E.screencols) == -1)
    die("getWindowSize");
  E.screenrows -= 2;
}

int main(int argc, char** argv) {
  enableRawMode();
  initEditor();
  if (argc >= 2) {
    editorOpen(argv[1]);
  }

  editorSetStatusMessage(
    "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

  while (1) {
    editorRefreshScreen();
    editorProcessKeypress();
  }

  return 0;
}
