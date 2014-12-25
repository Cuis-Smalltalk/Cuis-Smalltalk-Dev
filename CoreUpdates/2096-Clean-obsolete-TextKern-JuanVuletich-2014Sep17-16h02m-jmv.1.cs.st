'From Cuis 4.2 of 25 July 2013 [latest update: #2095] on 17 September 2014 at 4:03:06.009592 pm'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

TextModel allInstancesDo: [ :each | each flushUndoRedoCommands ].
TextModel allInstancesDo: [ :each | each actualContents runs  mapValues: [ :attributes | attributes reject: [ :att | att class isObsolete ]]].
Smalltalk garbageCollect.
Smalltalk obsoleteBehaviors print.
!

