module kilo;

import core.stdc.ctype;
import core.stdc.errno;
import core.sys.posix.fcntl;
import core.stdc.stdio;
import core.stdc.stdarg;
import core.stdc.stdlib;
import core.stdc.string;
import core.sys.posix.stdio : getline;
import core.sys.posix.sys.ioctl;
import core.sys.posix.sys.types;
import core.sys.posix.termios;
import core.stdc.time;
import core.sys.posix.unistd;
import std.conv : octal;

/*** defines ***/

enum KILO_VERSION = "0.0.1";
enum KILO_TAB_STOP = 8;
enum KILO_QUIT_TIMES = 3;

char CTRL_KEY(char k) @safe {
  return k & 0x1f;
}

enum : int {
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
alias editorKey = int;

enum : int {
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH
}
alias editorHighlight = int;

enum HL_HIGHLIGHT_NUMBERS = (1 << 0);
enum HL_HIGHLIGHT_STRINGS = (1 << 1);

/*** data ***/

struct editorSyntax {
  string filetype;
  string[] filematch;
  string[] keywords;
  string singleline_comment_start;
  string multiline_comment_start;
  string multiline_comment_end;
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
  char[80] statusmsg;
  time_t statusmsg_time;
  const(editorSyntax)* syntax;
  termios orig_termios;
}

editorConfig E;

/*** filetypes ***/

enum string[] C_HL_extensions = [".c", ".h", ".cpp", null];
enum string[] C_HL_keywords = [
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "class", "case",

    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", null
  ];

static const editorSyntax[] HLDB = [
  editorSyntax(
    "c",
    C_HL_extensions,
    C_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  ),
];

/*** terminal ***/

void die(const(char)* s) {
  write(STDOUT_FILENO, "\x1b[2J".ptr, 4);
  write(STDOUT_FILENO, "\x1b[H".ptr, 3);

  perror(s);
  exit(1);
}

extern (C) void disableRawMode() {
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
  long nread;
  char c;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN)
      die("read");
  }

  if (c == '\x1b') {
    char[3] seq;

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
  char[32] buf;
  uint i = 0;

  if (write(STDOUT_FILENO, "\x1b[6n".ptr, 4) != 4)
    return -1;

  while (i < buf.length - 1) {
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
  winsize ws;

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B".ptr, 12) != 12)
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
  // TODO replace strchr with import std.algorithm.searching : canFind;
  return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != null;
}

void editorUpdateSyntax(erow* row) {
  row.hl = cast(ubyte*) realloc(row.hl, row.rsize);
  memset(row.hl, HL_NORMAL, row.rsize);

  if (E.syntax == null)
    return;

  const string[] keywords = E.syntax.keywords;

  string scs = E.syntax.singleline_comment_start;
  string mcs = E.syntax.multiline_comment_start;
  string mce = E.syntax.multiline_comment_end;

  int prev_sep = 1;
  int in_string = 0;
  int in_comment = (row.idx > 0 && E.row[row.idx - 1].hl_open_comment);

  int i = 0;
  while (i < row.rsize) {
    char c = row.render[i];
    ubyte prev_hl = (i > 0) ? row.hl[i - 1] : HL_NORMAL;

    if (scs.length && !in_string && !in_comment) {
      if (!strncmp(&row.render[i], scs.ptr, scs.length)) {
        memset(&row.hl[i], HL_COMMENT, row.rsize - i);
        break;
      }
    }

    if (mcs.length && mce.length && !in_string) {
      if (in_comment) {
        row.hl[i] = HL_MLCOMMENT;
        if (!strncmp(&row.render[i], mce.ptr, mce.length)) {
          memset(&row.hl[i], HL_MLCOMMENT, mce.length);
          i += mce.length;
          in_comment = 0;
          prev_sep = 1;
          continue;
        }
        else {
          i++;
          continue;
        }
      }
      else if (!strncmp(&row.render[i], mcs.ptr, mcs.length)) {
        memset(&row.hl[i], HL_MLCOMMENT, mcs.length);
        i += mcs.length;
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
      int j;
      for (; keywords[j]; j++) {
        size_t klen = keywords[j].length;
        int kw2 = keywords[j][klen - 1] == '|';
        if (kw2)
          klen--;

        if (!strncmp(&row.render[i], keywords[j].ptr, klen) &&
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

int editorSyntaxToColor(int hl) @safe {
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

  foreach (const ref s; HLDB) {
    for (int i = 0; s.filematch[i]; ++i) {
      int is_ext = (s.filematch[i][0] == '.');
      if ((is_ext && ext && !strcmp(ext, s.filematch[i].ptr)) ||
        (!is_ext && strstr(E.filename, s.filematch[i].ptr))) {
        E.syntax = &s;

        foreach (filerow; 0 .. E.numrows) {
          editorUpdateSyntax(&E.row[filerow]);
        }

        return;
      }
    }
  }
}

/*** row operations ***/

int editorRowCxToRx(erow* row, int cx) {
  int rx = 0;
  foreach (j; 0 .. cx) {
    if (row.chars[j] == '\t')
      rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
    rx++;
  }
  return rx;
}

int editorRowRxToCx(erow* row, ptrdiff_t rx) {
  int cur_rx;
  int cx;
  for (; cx < row.size; cx++) {
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
  foreach (j; 0 .. row.size)
    if (row.chars[j] == '\t')
      tabs++;

  free(row.render);
  row.render = cast(char*) malloc(row.size + tabs * (KILO_TAB_STOP - 1) + 1);

  int idx = 0;
  foreach (j; 0 .. row.size) {
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

void editorInsertRow(int at, const(char)* s, int len) {
  if (at < 0 || at > E.numrows)
    return;

  E.row = cast(erow*) realloc(E.row, erow.sizeof * (E.numrows + 1));
  memmove(&E.row[at + 1], &E.row[at], erow.sizeof * (E.numrows - at));
  foreach (j; at + 1 .. E.numrows + 1)
    E.row[j].idx++;

  E.row[at].idx = at;

  E.row[at].size = len;
  E.row[at].chars = cast(char*) malloc(len + 1);
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
  memmove(&E.row[at], &E.row[at + 1], erow.sizeof * (E.numrows - at - 1));
  for (int j = at; j < E.numrows - 1; j++)
    E.row[j].idx--;
  E.numrows--;
  E.dirty++;
}

void editorRowInsertChar(erow* row, int at, char c) {
  if (at < 0 || at > row.size)
    at = row.size;
  row.chars = cast(char*) realloc(row.chars, row.size + 2);
  memmove(&row.chars[at + 1], &row.chars[at], row.size - at + 1);
  row.size++;
  row.chars[at] = c;
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowAppendString(erow* row, char* s, size_t len) {
  row.chars = cast(char*) realloc(row.chars, row.size + len + 1);
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

void editorInsertChar(char c) {
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
  foreach (j; 0 .. E.numrows)
    totlen += E.row[j].size + 1;
  *buflen = totlen;

  char* buf = cast(char*) malloc(totlen);
  char* p = buf;
  foreach (j; 0 .. E.numrows) {
    memcpy(p, E.row[j].chars, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }

  return buf;
}

void editorOpen(const(char)* filename) {
  free(E.filename);
  E.filename = strdup(filename);

  editorSelectSyntaxHighlight();

  FILE* fp = fopen(filename, "r");
  if (!fp)
    die("fopen");

  char* line = null;
  size_t linecap = 0;
  ssize_t linelen;
  while ((linelen = getline(&line, &linecap, fp)) != -1) {
    while (linelen > 0 && (line[linelen - 1] == '\n' ||
        line[linelen - 1] == '\r'))
      linelen--;
    // TODO: fix this cast.
    assert(int.min <= linelen && linelen <= int.max);
    editorInsertRow(E.numrows, cast(const(char)*) line, cast(int) linelen);
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

  int len;
  char* buf = editorRowsToString(&len);
  scope (exit)
    free(buf);

  int fd = open(E.filename, O_RDWR | O_CREAT, octal!644);
  scope (exit)
    close(fd);

  if (fd != -1 &&
    ftruncate(fd, len) != -1 &&
    write(fd, buf, len) == len) {
    E.dirty = 0;
    editorSetStatusMessage("%d bytes written to disk", len);
    return;
  }

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
  foreach (_; 0 .. E.numrows) {
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
      saved_hl = cast(char*) malloc(row.rsize);
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

void abAppend(abuf* ab, const(char)* s, long len) {
  char* new_ = cast(char*) realloc(ab.b, ab.len + len);

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
  foreach (y; 0 .. E.screenrows) {
    int filerow = y + E.rowoff;
    if (filerow >= E.numrows) {
      if (E.numrows == 0 && y == E.screenrows / 3) {
        char[80] welcome;
        int welcomelen = snprintf(welcome.ptr, welcome.length,
          "Kilo editor -- version %s", KILO_VERSION.ptr);
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
      foreach (j; 0 .. len) {
        if (iscntrl(c[j])) {
          char sym = (c[j] <= 26) ? cast(char)('@' + c[j]) : '?';
          abAppend(ab, "\x1b[7m", 4);
          abAppend(ab, &sym, 1);
          abAppend(ab, "\x1b[m", 3);
          if (current_color != -1) {
            char[16] buf;
            int clen = snprintf(buf.ptr, buf.length, "\x1b[%dm", current_color);
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
            char[16] buf;
            int clen = snprintf(buf.ptr, buf.length, "\x1b[%dm", color);
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
  char[80] status, rstatus;
  int len = snprintf(status.ptr, status.length, "%.20s - %d lines %s",
    E.filename ? E.filename : "[No Name]", E.numrows,
    E.dirty ? "(modified)".ptr : "".ptr);
  int rlen = snprintf(rstatus.ptr, rstatus.length, "%s | %d/%d",
    (E.syntax ? E.syntax.filetype : "no ft").ptr, E.cy + 1, E.numrows);
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
  size_t msglen = strlen(E.statusmsg.ptr);
  if (msglen > E.screencols)
    msglen = E.screencols;
  if (msglen && time(null) - E.statusmsg_time < 5)
    abAppend(ab, E.statusmsg.ptr, msglen);
}

void editorRefreshScreen() {
  editorScroll();

  abuf ab;

  abAppend(&ab, "\x1b[?25l", 6);
  abAppend(&ab, "\x1b[H", 3);

  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);

  char[32] buf;
  snprintf(buf.ptr, buf.length, "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
    (E.rx - E.coloff) + 1);
  abAppend(&ab, buf.ptr, strlen(buf.ptr));

  abAppend(&ab, "\x1b[?25h", 6);

  write(STDOUT_FILENO, ab.b, ab.len);
  abFree(&ab);
}

void editorSetStatusMessage(Args...)(const(char)* fmt, Args args) {
  snprintf(E.statusmsg.ptr, E.statusmsg.length, fmt, args);
  E.statusmsg_time = time(null);
}

/*** input ***/

char* editorPrompt(string prompt, void function(char*, int) callback) {
  size_t bufsize = 128;
  char* buf = cast(char*) malloc(bufsize);

  size_t buflen = 0;
  buf[0] = '\0';

  while (1) {
    editorSetStatusMessage(prompt.ptr, buf);
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
        buf = cast(char*) realloc(buf, bufsize);
      }
      assert(char.min <= c && c <= char.max);
      buf[buflen++] = cast(char) c;
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
    write(STDOUT_FILENO, "\x1b[2J".ptr, 4);
    write(STDOUT_FILENO, "\x1b[H".ptr, 3);
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
    assert(char.min <= c && c <= char.max);
    editorInsertChar(cast(char) c);
    break;
  }

  quit_times = KILO_QUIT_TIMES;
}

/*** init ***/

void initEditor() {
  if (getWindowSize(&E.screenrows, &E.screencols) == -1)
    die("getWindowSize");
  E.screenrows -= 2;
}

extern (C) int main(int argc, const(char)** argv) {
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
