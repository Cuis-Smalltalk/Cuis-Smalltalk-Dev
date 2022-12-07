'From Cuis 6.0 [latest update: #5570] on 5 December 2022 at 5:32:56 pm'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
TTFontDescription initialize.
TrueTypeFontFamily readTrueTypeFontEntry: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'DejaVu' / 'DejaVuSans' // 'DejaVuSans.ttf'.
TrueTypeFontFamily readTrueTypeFontEntry: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'DejaVu' / 'DejaVuSans' // 'DejaVuSans-Oblique.ttf'.
TrueTypeFontFamily readTrueTypeFontEntry: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'DejaVu' / 'DejaVuSans' // 'DejaVuSans-Bold.ttf'.
TrueTypeFontFamily readTrueTypeFontEntry: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'DejaVu' / 'DejaVuSans' // 'DejaVuSans-BoldOblique.ttf'.
[(Delay forSeconds: 1) wait. Smalltalk garbageCollect] fork!

