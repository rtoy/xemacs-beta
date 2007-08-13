;;; DO NOT MODIFY THIS FILE
(if (featurep 'calendar-autoloads) (error "Already loaded"))

;;;### (autoloads nil "cal-dst" "calendar/cal-dst.el")

(put 'calendar-daylight-savings-starts 'risky-local-variable t)

(put 'calendar-daylight-savings-ends 'risky-local-variable t)

;;;***

;;;### (autoloads nil "cal-x" "calendar/cal-x.el")

(defvar calendar-setup 'one-frame "\
The frame set up of the calendar.
The choices are `one-frame' (calendar and diary together in one separate,
dediciated frame) or `two-frames' (calendar and diary in separate, dedicated
frames); with any other value the current frame is used.")

;;;***

;;;### (autoloads (list-yahrzeit-dates calendar) "calendar" "calendar/calendar.el")

(defcustom calendar-week-start-day 0 "*The day of the week on which a week in the calendar begins.\n0 means Sunday (default), 1 means Monday, and so on." :type 'integer :group 'calendar)

(defcustom calendar-offset 0 "*The offset of the principal month from the center of the calendar window.\n0 means the principal month is in the center (default), -1 means on the left,\n+1 means on the right.  Larger (or smaller) values push the principal month off\nthe screen." :type 'integer :group 'calendar)

(defcustom view-diary-entries-initially nil "*Non-nil means display current date's diary entries on entry.\nThe diary is displayed in another window when the calendar is first displayed,\nif the current date is visible.  The number of days of diary entries displayed\nis governed by the variable `number-of-diary-entries'." :type 'boolean :group 'diary)

(defcustom number-of-diary-entries 1 "*Specifies how many days of diary entries are to be displayed initially.\nThis variable affects the diary display when the command M-x diary is used,\nor if the value of the variable `view-diary-entries-initially' is t.  For\nexample, if the default value 1 is used, then only the current day's diary\nentries will be displayed.  If the value 2 is used, then both the current\nday's and the next day's entries will be displayed.\n\nThe value can also be a vector such as [0 2 2 2 2 4 1]; this value\nsays to display no diary entries on Sunday, the display the entries\nfor the current date and the day after on Monday through Thursday,\ndisplay Friday through Monday's entries on Friday, and display only\nSaturday's entries on Saturday.\n\nThis variable does not affect the diary display with the `d' command\nfrom the calendar; in that case, the prefix argument controls the\nnumber of days of diary entries displayed." :type 'integer :group 'diary)

(defcustom mark-diary-entries-in-calendar nil "*Non-nil means mark dates with diary entries, in the calendar window.\nThe marking symbol is specified by the variable `diary-entry-marker'." :type 'boolean :group 'diary)

(defcustom view-calendar-holidays-initially nil "*Non-nil means display holidays for current three month period on entry.\nThe holidays are displayed in another window when the calendar is first\ndisplayed." :type 'boolean :group 'holidays)

(defcustom mark-holidays-in-calendar nil "*Non-nil means mark dates of holidays in the calendar window.\nThe marking symbol is specified by the variable `calendar-holiday-marker'." :type 'boolean :group 'holidays)

(defcustom all-hebrew-calendar-holidays nil "*If nil, show only major holidays from the Hebrew calendar.\nThis means only those Jewish holidays that appear on secular calendars.\n\nIf t, show all the holidays that would appear in a complete Hebrew calendar." :type 'boolean :group 'holidays)

(defcustom all-christian-calendar-holidays nil "*If nil, show only major holidays from the Christian calendar.\nThis means only those Christian holidays that appear on secular calendars.\n\nIf t, show all the holidays that would appear in a complete Christian\ncalendar." :type 'boolean :group 'holidays)

(defcustom all-islamic-calendar-holidays nil "*If nil, show only major holidays from the Islamic calendar.\nThis means only those Islamic holidays that appear on secular calendars.\n\nIf t, show all the holidays that would appear in a complete Islamic\ncalendar." :type 'boolean :group 'holidays)

(defcustom calendar-load-hook nil "*List of functions to be called after the calendar is first loaded.\nThis is the place to add key bindings to `calendar-mode-map'." :type 'hook :group 'calendar)

(defcustom initial-calendar-window-hook nil "*List of functions to be called when the calendar window is first opened.\nThe functions invoked are called after the calendar window is opened, but\nonce opened is never called again.  Leaving the calendar with the `q' command\nand reentering it will cause these functions to be called again." :type 'hook :group 'calendar)

(defcustom today-visible-calendar-hook nil "*List of functions called whenever the current date is visible.\nThis can be used, for example, to replace today's date with asterisks; a\nfunction `calendar-star-date' is included for this purpose:\n    (setq today-visible-calendar-hook 'calendar-star-date)\nIt can also be used to mark the current date with `calendar-today-marker';\na function is also provided for this:\n    (setq today-visible-calendar-hook 'calendar-mark-today)\n\nThe corresponding variable `today-invisible-calendar-hook' is the list of\nfunctions called when the calendar function was called when the current\ndate is not visible in the window.\n\nOther than the use of the provided functions, the changing of any\ncharacters in the calendar buffer by the hooks may cause the failure of the\nfunctions that move by days and weeks." :type 'hook :group 'calendar)

(defcustom today-invisible-calendar-hook nil "*List of functions called whenever the current date is not visible.\n\nThe corresponding variable `today-visible-calendar-hook' is the list of\nfunctions called when the calendar function was called when the current\ndate is visible in the window.\n\nOther than the use of the provided functions, the changing of any\ncharacters in the calendar buffer by the hooks may cause the failure of the\nfunctions that move by days and weeks." :type 'hook :group 'calendar)

(defcustom diary-file "~/diary" "*Name of the file in which one's personal diary of dates is kept.\n\nThe file's entries are lines in any of the forms\n\n            MONTH/DAY\n            MONTH/DAY/YEAR\n            MONTHNAME DAY\n            MONTHNAME DAY, YEAR\n            DAYNAME\n\nat the beginning of the line; the remainder of the line is the diary entry\nstring for that date.  MONTH and DAY are one or two digit numbers, YEAR is\na number and may be written in full or abbreviated to the final two digits.\nIf the date does not contain a year, it is generic and applies to any year.\nDAYNAME entries apply to any date on which is on that day of the week.\nMONTHNAME and DAYNAME can be spelled in full, abbreviated to three\ncharacters (with or without a period), capitalized or not.  Any of DAY,\nMONTH, or MONTHNAME, YEAR can be `*' which matches any day, month, or year,\nrespectively.\n\nThe European style (in which the day precedes the month) can be used\ninstead, if you execute `european-calendar' when in the calendar, or set\n`european-calendar-style' to t in your .emacs file.  The European forms are\n\n            DAY/MONTH\n            DAY/MONTH/YEAR\n            DAY MONTHNAME\n            DAY MONTHNAME YEAR\n            DAYNAME\n\nTo revert to the default American style from the European style, execute\n`american-calendar' in the calendar.\n\nA diary entry can be preceded by the character\n`diary-nonmarking-symbol' (ordinarily `&') to make that entry\nnonmarking--that is, it will not be marked on dates in the calendar\nwindow but will appear in a diary window.\n\nMultiline diary entries are made by indenting lines after the first with\neither a TAB or one or more spaces.\n\nLines not in one the above formats are ignored.  Here are some sample diary\nentries (in the default American style):\n\n     12/22/1988 Twentieth wedding anniversary!!\n     &1/1. Happy New Year!\n     10/22 Ruth's birthday.\n     21: Payday\n     Tuesday--weekly meeting with grad students at 10am\n              Supowit, Shen, Bitner, and Kapoor to attend.\n     1/13/89 Friday the thirteenth!!\n     &thu 4pm squash game with Lloyd.\n     mar 16 Dad's birthday\n     April 15, 1989 Income tax due.\n     &* 15 time cards due.\n\nIf the first line of a diary entry consists only of the date or day name with\nno trailing blanks or punctuation, then that line is not displayed in the\ndiary window; only the continuation lines is shown.  For example, the\nsingle diary entry\n\n     02/11/1989\n      Bill Blattner visits Princeton today\n      2pm Cognitive Studies Committee meeting\n      2:30-5:30 Lizzie at Lawrenceville for `Group Initiative'\n      4:00pm Jamie Tappenden\n      7:30pm Dinner at George and Ed's for Alan Ryan\n      7:30-10:00pm dance at Stewart Country Day School\n\nwill appear in the diary window without the date line at the beginning.  This\nfacility allows the diary window to look neater, but can cause confusion if\nused with more than one day's entries displayed.\n\nDiary entries can be based on Lisp sexps.  For example, the diary entry\n\n      %%(diary-block 11 1 1990 11 10 1990) Vacation\n\ncauses the diary entry \"Vacation\" to appear from November 1 through November\n10, 1990.  Other functions available are `diary-float', `diary-anniversary',\n`diary-cyclic', `diary-day-of-year', `diary-iso-date', `diary-french-date',\n`diary-hebrew-date', `diary-islamic-date', `diary-mayan-date',\n`diary-yahrzeit', `diary-sunrise-sunset', `diary-phases-of-moon',\n`diary-parasha', `diary-omer', `diary-rosh-hodesh', and\n`diary-sabbath-candles'.  See the documentation for the function\n`list-sexp-diary-entries' for more details.\n\nDiary entries based on the Hebrew and/or the Islamic calendar are also\npossible, but because these are somewhat slow, they are ignored\nunless you set the `nongregorian-diary-listing-hook' and the\n`nongregorian-diary-marking-hook' appropriately.  See the documentation\nfor these functions for details.\n\nDiary files can contain directives to include the contents of other files; for\ndetails, see the documentation for the variable `list-diary-entries-hook'." :type 'file :group 'diary)

(defcustom diary-nonmarking-symbol "&" "*Symbol indicating that a diary entry is not to be marked in the calendar." :type 'string :group 'diary)

(defcustom hebrew-diary-entry-symbol "H" "*Symbol indicating a diary entry according to the Hebrew calendar." :type 'string :group 'diary)

(defcustom islamic-diary-entry-symbol "I" "*Symbol indicating a diary entry according to the Islamic calendar." :type 'string :group 'diary)

(defcustom diary-include-string "#include" "*The string indicating inclusion of another file of diary entries.\nSee the documentation for the function `include-other-diary-files'." :type 'string :group 'diary)

(defcustom sexp-diary-entry-symbol "%%" "*The string used to indicate a sexp diary entry in diary-file.\nSee the documentation for the function `list-sexp-diary-entries'." :type 'string :group 'diary)

(defcustom abbreviated-calendar-year t "*Interpret a two-digit year DD in a diary entry as either 19DD or 20DD.\nFor the Gregorian calendar; similarly for the Hebrew and Islamic calendars.\nIf this variable is nil, years must be written in full." :type 'boolean :group 'diary)

(defcustom european-calendar-style nil "*Use the European style of dates in the diary and in any displays.\nIf this variable is t, a date 1/2/1990 would be interpreted as February 1,\n1990.  The accepted European date styles are\n\n            DAY/MONTH\n            DAY/MONTH/YEAR\n            DAY MONTHNAME\n            DAY MONTHNAME YEAR\n            DAYNAME\n\nNames can be capitalized or not, written in full, or abbreviated to three\ncharacters with or without a period." :type 'boolean :group 'diary)

(defcustom american-date-diary-pattern '((month "/" day "[^/0-9]") (month "/" day "/" year "[^0-9]") (monthname " *" day "[^,0-9]") (monthname " *" day ", *" year "[^0-9]") (dayname "\\W")) "*List of pseudo-patterns describing the American patterns of date used.\nSee the documentation of `diary-date-forms' for an explanation." :type '(repeat (choice (cons :tag "Backup" (const backup) (repeat (list :inline t :format "%v" (symbol :tag "Keyword") (choice symbol regexp)))) (repeat (list :inline t :format "%v" (symbol :tag "Keyword") (choice symbol regexp))))) :group 'diary)

(defcustom european-date-diary-pattern '((day "/" month "[^/0-9]") (day "/" month "/" year "[^0-9]") (backup day " *" monthname "\\W+\\<[^*0-9]") (day " *" monthname " *" year "[^0-9]") (dayname "\\W")) "*List of pseudo-patterns describing the European patterns of date used.\nSee the documentation of `diary-date-forms' for an explanation." :type '(repeat (choice (cons :tag "Backup" (const backup) (repeat (list :inline t :format "%v" (symbol :tag "Keyword") (choice symbol regexp)))) (repeat (list :inline t :format "%v" (symbol :tag "Keyword") (choice symbol regexp))))) :group 'diary)

(defcustom european-calendar-display-form '((if dayname (concat dayname ", ")) day " " monthname " " year) "*Pseudo-pattern governing the way a date appears in the European style.\nSee the documentation of calendar-date-display-form for an explanation." :type 'sexp :group 'calendar)

(defcustom american-calendar-display-form '((if dayname (concat dayname ", ")) monthname " " day ", " year) "*Pseudo-pattern governing the way a date appears in the American style.\nSee the documentation of `calendar-date-display-form' for an explanation." :type 'sexp :group 'calendar)

(defcustom print-diary-entries-hook 'lpr-buffer "*List of functions called after a temporary diary buffer is prepared.\nThe buffer shows only the diary entries currently visible in the diary\nbuffer.  The default just does the printing.  Other uses might include, for\nexample, rearranging the lines into order by day and time, saving the buffer\ninstead of deleting it, or changing the function used to do the printing." :type 'hook :group 'diary)

(defcustom list-diary-entries-hook nil "*List of functions called after diary file is culled for relevant entries.\nIt is to be used for diary entries that are not found in the diary file.\n\nA function `include-other-diary-files' is provided for use as the value of\nthis hook.  This function enables you to use shared diary files together\nwith your own.  The files included are specified in the diary file by lines\nof the form\n\n        #include \"filename\"\n\nThis is recursive; that is, #include directives in files thus included are\nobeyed.  You can change the \"#include\" to some other string by changing\nthe variable `diary-include-string'.  When you use `include-other-diary-files'\nas part of the list-diary-entries-hook, you will probably also want to use the\nfunction `mark-included-diary-files' as part of `mark-diary-entries-hook'.\n\nFor example, you could use\n\n     (setq list-diary-entries-hook\n       '(include-other-diary-files sort-diary-entries))\n     (setq diary-display-hook 'fancy-diary-display)\n\nin your `.emacs' file to cause the fancy diary buffer to be displayed with\ndiary entries from various included files, each day's entries sorted into\nlexicographic order." :type 'hook :group 'diary)

(defcustom diary-hook nil "*List of functions called after the display of the diary.\nCan be used for appointment notification." :type 'hook :group 'diary)

(defcustom diary-display-hook nil "*List of functions that handle the display of the diary.\nIf nil (the default), `simple-diary-display' is used.  Use `ignore' for no\ndiary display.\n\nOrdinarily, this just displays the diary buffer (with holidays indicated in\nthe mode line), if there are any relevant entries.  At the time these\nfunctions are called, the variable `diary-entries-list' is a list, in order\nby date, of all relevant diary entries in the form of ((MONTH DAY YEAR)\nSTRING), where string is the diary entry for the given date.  This can be\nused, for example, a different buffer for display (perhaps combined with\nholidays), or produce hard copy output.\n\nA function `fancy-diary-display' is provided as an alternative\nchoice for this hook; this function prepares a special noneditable diary\nbuffer with the relevant diary entries that has neat day-by-day arrangement\nwith headings.  The fancy diary buffer will show the holidays unless the\nvariable `holidays-in-diary-buffer' is set to nil.  Ordinarily, the fancy\ndiary buffer will not show days for which there are no diary entries, even\nif that day is a holiday; if you want such days to be shown in the fancy\ndiary buffer, set the variable `diary-list-include-blanks' to t." :type 'hook :group 'diary)

(defcustom nongregorian-diary-listing-hook nil "*List of functions called for listing diary file and included files.\nAs the files are processed for diary entries, these functions are used to cull\nrelevant entries.  You can use either or both of `list-hebrew-diary-entries'\nand `list-islamic-diary-entries'.  The documentation for these functions\ndescribes the style of such diary entries." :type 'hook :group 'diary)

(defcustom mark-diary-entries-hook nil "*List of functions called after marking diary entries in the calendar.\n\nA function `mark-included-diary-files' is also provided for use as the\nmark-diary-entries-hook; it enables you to use shared diary files together\nwith your own.  The files included are specified in the diary file by lines\nof the form\n        #include \"filename\"\nThis is recursive; that is, #include directives in files thus included are\nobeyed.  You can change the \"#include\" to some other string by changing the\nvariable `diary-include-string'.  When you use `mark-included-diary-files' as\npart of the mark-diary-entries-hook, you will probably also want to use the\nfunction `include-other-diary-files' as part of `list-diary-entries-hook'." :type 'hook :group 'diary)

(defcustom nongregorian-diary-marking-hook nil "*List of functions called for marking diary file and included files.\nAs the files are processed for diary entries, these functions are used to cull\nrelevant entries.  You can use either or both of `mark-hebrew-diary-entries'\nand `mark-islamic-diary-entries'.  The documentation for these functions\ndescribes the style of such diary entries." :type 'hook :group 'diary)

(defcustom diary-list-include-blanks nil "*If nil, do not include days with no diary entry in the list of diary entries.\nSuch days will then not be shown in the fancy diary buffer, even if they\nare holidays." :type 'boolean :group 'diary)

(defcustom holidays-in-diary-buffer t "*Non-nil means include holidays in the diary display.\nThe holidays appear in the mode line of the diary buffer, or in the\nfancy diary buffer next to the date.  This slows down the diary functions\nsomewhat; setting it to nil makes the diary display faster." :type 'boolean :group 'diary)

(defcustom general-holidays '((holiday-fixed 1 1 "New Year's Day") (holiday-float 1 1 3 "Martin Luther King Day") (holiday-fixed 2 2 "Ground Hog Day") (holiday-fixed 2 14 "Valentine's Day") (holiday-float 2 1 3 "President's Day") (holiday-fixed 3 17 "St. Patrick's Day") (holiday-fixed 4 1 "April Fool's Day") (holiday-float 5 0 2 "Mother's Day") (holiday-float 5 1 -1 "Memorial Day") (holiday-fixed 6 14 "Flag Day") (holiday-float 6 0 3 "Father's Day") (holiday-fixed 7 4 "Independence Day") (holiday-float 9 1 1 "Labor Day") (holiday-float 10 1 2 "Columbus Day") (holiday-fixed 10 31 "Halloween") (holiday-fixed 11 11 "Veteran's Day") (holiday-float 11 4 4 "Thanksgiving")) "*General holidays.  Default value is for the United States.\nSee the documentation for `calendar-holidays' for details." :type 'sexp :group 'holidays)

(put 'general-holidays 'risky-local-variable t)

(defcustom local-holidays nil "*Local holidays.\nSee the documentation for `calendar-holidays' for details." :type 'sexp :group 'holidays :group 'local)

(put 'local-holidays 'risky-local-variable t)

(defcustom other-holidays nil "*User defined holidays.\nSee the documentation for `calendar-holidays' for details." :type 'sexp :group 'holidays)

(put 'other-holidays 'risky-local-variable t)

(defvar hebrew-holidays-1 '((holiday-rosh-hashanah-etc) (if all-hebrew-calendar-holidays (holiday-julian 11 (let* ((m displayed-month) (y displayed-year) (year)) (increment-calendar-month m y -1) (let ((year (extract-calendar-year (calendar-julian-from-absolute (calendar-absolute-from-gregorian (list m 1 y)))))) (if (zerop (% (1+ year) 4)) 22 21))) "\"Tal Umatar\" (evening)"))))

(put 'hebrew-holidays-1 'risky-local-variable t)

(defvar hebrew-holidays-2 '((if all-hebrew-calendar-holidays (holiday-hanukkah) (holiday-hebrew 9 25 "Hanukkah")) (if all-hebrew-calendar-holidays (holiday-hebrew 10 (let ((h-year (extract-calendar-year (calendar-hebrew-from-absolute (calendar-absolute-from-gregorian (list displayed-month 28 displayed-year)))))) (if (= (% (calendar-absolute-from-hebrew (list 10 10 h-year)) 7) 6) 11 10)) "Tzom Teveth")) (if all-hebrew-calendar-holidays (holiday-hebrew 11 15 "Tu B'Shevat"))))

(put 'hebrew-holidays-2 'risky-local-variable t)

(defvar hebrew-holidays-3 '((if all-hebrew-calendar-holidays (holiday-hebrew 11 (let ((m displayed-month) (y displayed-year)) (increment-calendar-month m y 1) (let* ((h-year (extract-calendar-year (calendar-hebrew-from-absolute (calendar-absolute-from-gregorian (list m (calendar-last-day-of-month m y) y))))) (s-s (calendar-hebrew-from-absolute (if (= (% (calendar-absolute-from-hebrew (list 7 1 h-year)) 7) 6) (calendar-dayname-on-or-before 6 (calendar-absolute-from-hebrew (list 11 17 h-year))) (calendar-dayname-on-or-before 6 (calendar-absolute-from-hebrew (list 11 16 h-year)))))) (day (extract-calendar-day s-s))) day)) "Shabbat Shirah"))))

(put 'hebrew-holidays-3 'risky-local-variable t)

(defvar hebrew-holidays-4 '((holiday-passover-etc) (if (and all-hebrew-calendar-holidays (let* ((m displayed-month) (y displayed-year) (year)) (increment-calendar-month m y -1) (let ((year (extract-calendar-year (calendar-julian-from-absolute (calendar-absolute-from-gregorian (list m 1 y)))))) (= 21 (% year 28))))) (holiday-julian 3 26 "Kiddush HaHamah")) (if all-hebrew-calendar-holidays (holiday-tisha-b-av-etc))))

(put 'hebrew-holidays-4 'risky-local-variable t)

(defvar hebrew-holidays (append hebrew-holidays-1 hebrew-holidays-2 hebrew-holidays-3 hebrew-holidays-4) "\
*Jewish holidays.
See the documentation for `calendar-holidays' for details.")

(put 'hebrew-holidays 'risky-local-variable t)

(defvar christian-holidays '((if all-christian-calendar-holidays (holiday-fixed 1 6 "Epiphany")) (holiday-easter-etc) (if all-christian-calendar-holidays (holiday-greek-orthodox-easter)) (if all-christian-calendar-holidays (holiday-fixed 8 15 "Assumption")) (if all-christian-calendar-holidays (holiday-advent)) (holiday-fixed 12 25 "Christmas") (if all-christian-calendar-holidays (holiday-julian 12 25 "Eastern Orthodox Christmas"))) "\
*Christian holidays.
See the documentation for `calendar-holidays' for details.")

(put 'christian-holidays 'risky-local-variable t)

(defvar islamic-holidays '((holiday-islamic 1 1 (format "Islamic New Year %d" (let ((m displayed-month) (y displayed-year)) (increment-calendar-month m y 1) (extract-calendar-year (calendar-islamic-from-absolute (calendar-absolute-from-gregorian (list m (calendar-last-day-of-month m y) y))))))) (if all-islamic-calendar-holidays (holiday-islamic 1 10 "Ashura")) (if all-islamic-calendar-holidays (holiday-islamic 3 12 "Mulad-al-Nabi")) (if all-islamic-calendar-holidays (holiday-islamic 7 26 "Shab-e-Mi'raj")) (if all-islamic-calendar-holidays (holiday-islamic 8 15 "Shab-e-Bara't")) (holiday-islamic 9 1 "Ramadan Begins") (if all-islamic-calendar-holidays (holiday-islamic 9 27 "Shab-e Qadr")) (if all-islamic-calendar-holidays (holiday-islamic 10 1 "Id-al-Fitr")) (if all-islamic-calendar-holidays (holiday-islamic 12 10 "Id-al-Adha"))) "\
*Islamic holidays.
See the documentation for `calendar-holidays' for details.")

(put 'islamic-holidays 'risky-local-variable t)

(defvar solar-holidays '((if (fboundp 'atan) (solar-equinoxes-solstices)) (if (progn (require 'cal-dst) t) (funcall 'holiday-sexp calendar-daylight-savings-starts '(format "Daylight Savings Time Begins %s" (if (fboundp 'atan) (solar-time-string (/ calendar-daylight-savings-starts-time (float 60)) calendar-standard-time-zone-name) "")))) (funcall 'holiday-sexp calendar-daylight-savings-ends '(format "Daylight Savings Time Ends %s" (if (fboundp 'atan) (solar-time-string (/ calendar-daylight-savings-ends-time (float 60)) calendar-daylight-time-zone-name) "")))) "\
*Sun-related holidays.
See the documentation for `calendar-holidays' for details.")

(put 'solar-holidays 'risky-local-variable t)

(defvar calendar-holidays (append general-holidays local-holidays other-holidays christian-holidays hebrew-holidays islamic-holidays solar-holidays) "\
*List of notable days for the command M-x holidays.

Additional holidays are easy to add to the list, just put them in the list
`other-holidays' in your .emacs file.  Similarly, by setting any of
`general-holidays', `local-holidays' `christian-holidays', `hebrew-holidays',
`islamic-holidays', or `solar-holidays' to nil in your .emacs file, you can
eliminate unwanted categories of holidays.  The intention is that (in the US)
`local-holidays' be set in site-init.el and `other-holidays' be set by the
user.

Entries on the list are expressions that return (possibly empty) lists of
items of the form ((month day year) string) of a holiday in the in the
three-month period centered around `displayed-month' of `displayed-year'.
Several basic functions are provided for this purpose:

    (holiday-fixed MONTH DAY STRING) is a fixed date on the Gregorian calendar
    (holiday-float MONTH DAYNAME K STRING &optional day) is the Kth DAYNAME in
                               MONTH on the Gregorian calendar (0 for Sunday,
                               etc.); K<0 means count back from the end of the
                               month. An optional parameter DAY means the Kth
                               DAYNAME after/before MONTH DAY.
    (holiday-hebrew MONTH DAY STRING)  a fixed date on the Hebrew calendar
    (holiday-islamic MONTH DAY STRING) a fixed date on the Islamic calendar
    (holiday-julian MONTH DAY STRING)  a fixed date on the Julian calendar
    (holiday-sexp SEXP STRING) SEXP is a Gregorian-date-valued expression
                               in the variable `year'; if it evaluates to
                               a visible date, that's the holiday; if it
                               evaluates to nil, there's no holiday.  STRING
                               is an expression in the variable `date'.

For example, to add Bastille Day, celebrated in France on July 14, add

     (holiday-fixed 7 14 \"Bastille Day\")

to the list.  To add Hurricane Supplication Day, celebrated in the Virgin
Islands on the fourth Monday in August, add

     (holiday-float 8 1 4 \"Hurricane Supplication Day\")

to the list (the last Monday would be specified with `-1' instead of `4').
To add the last day of Hanukkah to the list, use

     (holiday-hebrew 10 2 \"Last day of Hanukkah\")

since the Hebrew months are numbered with 1 starting from Nisan, while to
add the Islamic feast celebrating Mohammed's birthday use

     (holiday-islamic 3 12 \"Mohammed's Birthday\")

since the Islamic months are numbered from 1 starting with Muharram.  To
add Thomas Jefferson's birthday, April 2, 1743 (Julian), use

     (holiday-julian 4 2 \"Jefferson's Birthday\")

To include a holiday conditionally, use the sexp form or a conditional.  For
example, to include American presidential elections, which occur on the first
Tuesday after the first Monday in November of years divisible by 4, add

     (holiday-sexp
       (if (zerop (% year 4))
           (calendar-gregorian-from-absolute
             (1+ (calendar-dayname-on-or-before
                   1 (+ 6 (calendar-absolute-from-gregorian
                            (list 11 1 year)))))))
       \"US Presidential Election\")

or

     (if (zerop (% displayed-year 4))
         (holiday-fixed 11
                (extract-calendar-day
                 (calendar-gregorian-from-absolute
                  (1+ (calendar-dayname-on-or-before
                       1 (+ 6 (calendar-absolute-from-gregorian
                               (list 11 1 displayed-year)))))))
                \"US Presidential Election\"))

to the list.  To include the phases of the moon, add

     (lunar-phases)

to the holiday list, where `lunar-phases' is an Emacs-Lisp function that
you've written to return a (possibly empty) list of the relevant VISIBLE dates
with descriptive strings such as

     (((2 6 1989) \"New Moon\") ((2 12 1989) \"First Quarter Moon\") ... ).")

(put 'calendar-holidays 'risky-local-variable t)

(autoload 'calendar "calendar" "\
Display a three-month calendar in another window.
The three months appear side by side, with the current month in the middle
surrounded by the previous and next months.  The cursor is put on today's date.

If called with an optional prefix argument, prompts for month and year.

This function is suitable for execution in a .emacs file; appropriate setting
of the variable `view-diary-entries-initially' will cause the diary entries for
the current date to be displayed in another window.  The value of the variable
`number-of-diary-entries' controls the number of days of diary entries
displayed upon initial display of the calendar.

An optional prefix argument ARG causes the calendar displayed to be ARG
months in the future if ARG is positive or in the past if ARG is negative;
in this case the cursor goes on the first day of the month.

Once in the calendar window, future or past months can be moved into view.
Arbitrary months can be displayed, or the calendar can be scrolled forward
or backward.

The cursor can be moved forward or backward by one day, one week, one month,
or one year.  All of these commands take prefix arguments which, when negative,
cause movement in the opposite direction.  For convenience, the digit keys
and the minus sign are automatically prefixes.  The window is replotted as
necessary to display the desired date.

Diary entries can be marked on the calendar or displayed in another window.

Use M-x describe-mode for details of the key bindings in the calendar window.

The Gregorian calendar is assumed.

After loading the calendar, the hooks given by the variable
`calendar-load-hook' are run.  This is the place to add key bindings to the
calendar-mode-map.

After preparing the calendar window initially, the hooks given by the variable
`initial-calendar-window-hook' are run.

The hooks given by the variable `today-visible-calendar-hook' are run
everytime the calendar window gets scrolled, if the current date is visible
in the window.  If it is not visible, the hooks given by the variable
`today-invisible-calendar-hook' are run.  Thus, for example, setting
`today-visible-calendar-hook' to 'calendar-star-date will cause today's date
to be replaced by asterisks to highlight it whenever it is in the window." t nil)

(autoload 'list-yahrzeit-dates "calendar" "\
List Yahrzeit dates for *Gregorian* DEATH-DATE from START-YEAR to END-YEAR.
When called interactively from the calendar window, the date of death is taken
from the cursor position." t nil)

;;;***

;;;### (autoloads (diary) "diary-lib" "calendar/diary-lib.el")

(autoload 'diary "diary-lib" "\
Generate the diary window for ARG days starting with the current date.
If no argument is provided, the number of days of diary entries is governed
by the variable `number-of-diary-entries'.  This function is suitable for
execution in a `.emacs' file." t nil)

;;;***

;;;### (autoloads (holidays) "holidays" "calendar/holidays.el")

(autoload 'holidays "holidays" "\
Display the holidays for last month, this month, and next month.
If called with an optional prefix argument, prompts for month and year.

This function is suitable for execution in a .emacs file." t nil)

;;;***

;;;### (autoloads (phases-of-moon) "lunar" "calendar/lunar.el")

(autoload 'phases-of-moon "lunar" "\
Display the quarters of the moon for last month, this month, and next month.
If called with an optional prefix argument, prompts for month and year.

This function is suitable for execution in a .emacs file." t nil)

;;;***

;;;### (autoloads (solar-equinoxes-solstices sunrise-sunset) "solar" "calendar/solar.el")

(defvar calendar-time-display-form '(12-hours ":" minutes am-pm (if time-zone " (") time-zone (if time-zone ")")) "\
*The pseudo-pattern that governs the way a time of day is formatted.

A pseudo-pattern is a list of expressions that can involve the keywords
`12-hours', `24-hours', and `minutes',  all numbers in string form,
and `am-pm' and `time-zone',  both alphabetic strings.

For example, the form

  '(24-hours \":\" minutes
    (if time-zone \" (\") time-zone (if time-zone \")\"))

would give military-style times like `21:07 (UTC)'.")

(defvar calendar-latitude nil "\
*Latitude of `calendar-location-name' in degrees.

The value can be either a decimal fraction (one place of accuracy is
sufficient), + north, - south, such as 40.7 for New York City, or the value
can be a vector [degrees minutes north/south] such as [40 50 north] for New
York City.

This variable should be set in site-local.el.")

(defvar calendar-longitude nil "\
*Longitude of `calendar-location-name' in degrees.

The value can be either a decimal fraction (one place of accuracy is
sufficient), + east, - west, such as -73.9 for New York City, or the value
can be a vector [degrees minutes east/west] such as [73 55 west] for New
York City.

This variable should be set in site-local.el.")

(defvar calendar-location-name '(let ((float-output-format "%.1f")) (format "%s%s, %s%s" (if (numberp calendar-latitude) (abs calendar-latitude) (+ (aref calendar-latitude 0) (/ (aref calendar-latitude 1) 60.0))) (if (numberp calendar-latitude) (if (> calendar-latitude 0) "N" "S") (if (equal (aref calendar-latitude 2) 'north) "N" "S")) (if (numberp calendar-longitude) (abs calendar-longitude) (+ (aref calendar-longitude 0) (/ (aref calendar-longitude 1) 60.0))) (if (numberp calendar-longitude) (if (> calendar-longitude 0) "E" "W") (if (equal (aref calendar-latitude 2) 'east) "E" "W")))) "\
*Expression evaluating to name of `calendar-longitude', calendar-latitude'.
For example, \"New York City\".  Default value is just the latitude, longitude
pair.

This variable should be set in site-local.el.")

(autoload 'sunrise-sunset "solar" "\
Local time of sunrise and sunset for today.  Accurate to +/- 2 minutes.
If called with an optional prefix argument, prompt for date.

If called with an optional double prefix argument, prompt for longitude,
latitude, time zone, and date, and always use standard time.

This function is suitable for execution in a .emacs file." t nil)

(autoload 'solar-equinoxes-solstices "solar" "\
Date and time of equinoxes and solstices, if visible in the calendar window.
Requires floating point." nil nil)

;;;***

(provide 'calendar-autoloads)
