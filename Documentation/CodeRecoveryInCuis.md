# Code Recovery in Cuis

Smalltalk systems store source code in two files, the Sources file (named, for example 'Cuis7.2.sources') and the Changes file (for example, 'Cuis7.3-7235.changes'). The Sources file is not modified after creation, and new and modified code is appended to the Changes file. Both these files are used (for instance) by the Smalltalk Browser, when exploring the source code for the system.

Cuis saves an additional type of files, UserChanges (named, for example, Cuis7.3-7235.user.002.changes), in the -UserFiles/UserChanges folder. These files are numbered, and they are created anew each time the Cuis system is started. These files are not used by the system to retrive code, they only exist to aid programmers. They will only include our work, and will conveniently use a different file for each programming session. They are not deleted automatically, and the user is free to delete them when desired.

To recover our code, the [recent changes] button in FileList will open a ChangeList on the selected file. This tool lets you review your work. For this, you can compare the various versions of methods in the file with their in-memory counterpart, filter them according to several criteria, load them as desired, etc.
