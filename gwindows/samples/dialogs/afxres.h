#define LANG_ENGLISH                     0x09
#define SUBLANG_ENGLISH_US               0x01    // English (USA)

#define WS_OVERLAPPED       0x00000000L
#define WS_POPUP            0x80000000L
#define WS_CHILD            0x40000000L
#define WS_MINIMIZE         0x20000000L
#define WS_VISIBLE          0x10000000L
#define WS_DISABLED         0x08000000L
#define WS_CLIPSIBLINGS     0x04000000L
#define WS_CLIPCHILDREN     0x02000000L
#define WS_MAXIMIZE         0x01000000L
#define WS_CAPTION          0x00C00000L
#define WS_BORDER           0x00800000L
#define WS_DLGFRAME         0x00400000L
#define WS_VSCROLL          0x00200000L
#define WS_HSCROLL          0x00100000L
#define WS_SYSMENU          0x00080000L
#define WS_THICKFRAME       0x00040000L
#define WS_GROUP            0x00020000L
#define WS_TABSTOP          0x00010000L

#define WS_MINIMIZEBOX      0x00020000L
#define WS_MAXIMIZEBOX      0x00010000L

#define WS_EX_CLIENTEDGE        0x00000200L

#define DS_ABSALIGN         0x01L
#define DS_SYSMODAL         0x02L
#define DS_LOCALEDIT        0x20L
#define DS_SETFONT          0x40L
#define DS_MODALFRAME       0x80L
#define DS_NOIDLEMSG        0x100L
#define DS_SETFOREGROUND    0x200L

#define ES_AUTOHSCROLL	(0x80L)
#define ES_AUTOVSCROLL	(0x40L)
#define ES_CENTER	(0x1L)
#define ES_LEFT	(0L)
#define ES_LOWERCASE	(0x10L)
#define ES_MULTILINE	(0x4L)
#define ES_NOHIDESEL	(0x100L)
#define ES_NUMBER	(0x2000L)
#define ES_OEMCONVERT	(0x400L)
#define ES_PASSWORD	(0x20L)
#define ES_READONLY	(0x800L)
#define ES_RIGHT	(0x2L)
#define ES_UPPERCASE	(0x8L)
#define ES_WANTRETURN	(0x1000L)

#define IDOK                1
#define IDCANCEL            2
#define IDABORT             3
#define IDRETRY             4
#define IDIGNORE            5
#define IDYES               6
#define IDNO                7
#define IDCLOSE             8
#define IDHELP              9
