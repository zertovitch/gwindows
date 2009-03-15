--  Standard IDs used for menus

package Standard_IDs is

   ID_FILE_NEW                     : constant := 16#E100#;
   ID_FILE_OPEN                    : constant := 16#E101#;
   ID_FILE_CLOSE                   : constant := 16#E102#;
   ID_FILE_SAVE                    : constant := 16#E103#;
   ID_FILE_SAVE_AS                 : constant := 16#E104#;
   ID_FILE_PAGE_SETUP              : constant := 16#E105#;
   ID_FILE_PRINT_SETUP             : constant := 16#E106#;
   ID_FILE_PRINT                   : constant := 16#E107#;
   ID_FILE_PRINT_DIRECT            : constant := 16#E108#;
   ID_FILE_PRINT_PREVIEW           : constant := 16#E109#;
   ID_FILE_UPDATE                  : constant := 16#E10A#;
   ID_FILE_SAVE_COPY_AS            : constant := 16#E10B#;
   ID_FILE_SEND_MAIL               : constant := 16#E10C#;

   ID_FILE_MRU_FIRST               : constant := 16#E110#;
   ID_FILE_MRU_FILE1               : constant := 16#E110#;
   ID_FILE_MRU_FILE2               : constant := 16#E111#;
   ID_FILE_MRU_FILE3               : constant := 16#E112#;
   ID_FILE_MRU_FILE4               : constant := 16#E113#;
   ID_FILE_MRU_FILE5               : constant := 16#E114#;
   ID_FILE_MRU_FILE6               : constant := 16#E115#;
   ID_FILE_MRU_FILE7               : constant := 16#E116#;
   ID_FILE_MRU_FILE8               : constant := 16#E117#;
   ID_FILE_MRU_FILE9               : constant := 16#E118#;
   ID_FILE_MRU_FILE10              : constant := 16#E119#;
   ID_FILE_MRU_FILE11              : constant := 16#E11A#;
   ID_FILE_MRU_FILE12              : constant := 16#E11B#;
   ID_FILE_MRU_FILE13              : constant := 16#E11C#;
   ID_FILE_MRU_FILE14              : constant := 16#E11D#;
   ID_FILE_MRU_FILE15              : constant := 16#E11E#;
   ID_FILE_MRU_FILE16              : constant := 16#E11F#;
   ID_FILE_MRU_LAST                : constant := 16#E11F#;

   --  Edit commands
   ID_EDIT_CLEAR                   : constant := 16#E120#;
   ID_EDIT_CLEAR_ALL               : constant := 16#E121#;
   ID_EDIT_COPY                    : constant := 16#E122#;
   ID_EDIT_CUT                     : constant := 16#E123#;
   ID_EDIT_FIND                    : constant := 16#E124#;
   ID_EDIT_PASTE                   : constant := 16#E125#;
   ID_EDIT_PASTE_LINK              : constant := 16#E126#;
   ID_EDIT_PASTE_SPECIAL           : constant := 16#E127#;
   ID_EDIT_REPEAT                  : constant := 16#E128#;
   ID_EDIT_REPLACE                 : constant := 16#E129#;
   ID_EDIT_SELECT_ALL              : constant := 16#E12A#;
   ID_EDIT_UNDO                    : constant := 16#E12B#;
   ID_EDIT_REDO                    : constant := 16#E12C#;

   --  Window commands
   ID_WINDOW_NEW                   : constant := 16#E130#;
   ID_WINDOW_ARRANGE               : constant := 16#E131#;
   ID_WINDOW_CASCADE               : constant := 16#E132#;
   ID_WINDOW_TILE_HORZ             : constant := 16#E133#;
   ID_WINDOW_TILE_VERT             : constant := 16#E134#;
   ID_WINDOW_SPLIT                 : constant := 16#E135#;
   ID_WINDOW_CLOSE_ALL             : constant := 16#E136#;

   --  Help and App commands
   ID_APP_ABOUT                    : constant := 16#E140#;
   ID_APP_EXIT                     : constant := 16#E141#;
   ID_HELP_INDEX                   : constant := 16#E142#;
   ID_HELP_FINDER                  : constant := 16#E143#;
   ID_HELP_USING                   : constant := 16#E144#;
   ID_CONTEXT_HELP                 : constant := 16#E145#;    -- shift-F1

   --  Special commands for processing help
   ID_HELP                         : constant := 16#E146#;
   --  First attempt for F1
   ID_DEFAULT_HELP                 : constant := 16#E147#;
   --  Last attempt

   --  Misc
   ID_NEXT_PANE                    : constant := 16#E150#;
   ID_PREV_PANE                    : constant := 16#E151#;

   --  Format
   ID_FORMAT_FONT                  : constant := 16#E160#;

   --  OLE commands
   ID_OLE_INSERT_NEW               : constant := 16#E200#;
   ID_OLE_EDIT_LINKS               : constant := 16#E201#;
   ID_OLE_EDIT_CONVERT             : constant := 16#E202#;
   ID_OLE_EDIT_CHANGE_ICON         : constant := 16#E203#;
   ID_OLE_EDIT_PROPERTIES          : constant := 16#E204#;
   ID_OLE_VERB_FIRST               : constant := 16#E210#;   -- range - 16 max
   ID_OLE_VERB_LAST                : constant := 16#E21F#;

   --  View Commands
   ID_VIEW_TOOLBAR                 : constant := 16#E800#;
   ID_VIEW_STATUS_BAR              : constant := 16#E801#;
   ID_VIEW_REBAR                   : constant := 16#E804#;
   ID_VIEW_AUTOARRANGE             : constant := 16#E805#;
   ID_VIEW_SMALLICON               : constant := 16#E810#;
   ID_VIEW_LARGEICON               : constant := 16#E811#;
   ID_VIEW_LIST                    : constant := 16#E812#;
   ID_VIEW_DETAILS                 : constant := 16#E813#;
   ID_VIEW_LINEUP                  : constant := 16#E814#;
   ID_VIEW_BYNAME                  : constant := 16#E815#;

   --  RecordForm commands
   ID_RECORD_FIRST                 : constant := 16#E900#;
   ID_RECORD_LAST                  : constant := 16#E901#;
   ID_RECORD_NEXT                  : constant := 16#E902#;
   ID_RECORD_PREV                  : constant := 16#E903#;

end Standard_IDs;
