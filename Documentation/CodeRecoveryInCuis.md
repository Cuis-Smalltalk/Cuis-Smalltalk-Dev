# Code Recovery in Cuis

Smalltalk systems store source code in two files, the Sources file (named, for example 'CuisV6.sources') and the Changes file (for example, 'Cuis6.0-5722.changes'). The Sources file is not modified after creation, and new and modified code is appended to the Changes file. Both these files are used (for instance) by the Smalltalk Browser, when exploring the source code for the system.

In the event of a system crash, it is generally advised to recover code from the Changes file. However, as system updates and optional packages are loaded, the Changes file will include other code in addition that the code we wrote or modified ourselves. This means that finding our work can become very difficult.

For this reason, Cuis saves an additional type of files, UserChanges (named, for example, Cuis6.0-5722.user.001.changes). These files are numbered, and they are created anew each time the Cuis system is started. These files are not used to retrive code for the system, they only exist to aid programmers. They will only include our work, and will conveniently use a different file for each programming session. They are not deleted automatically, and the user is free to delete them when desired.

To recover our code, the [recent changes] button in FileList will open a ChangeList on the selected file. This tool lets you review your work. For this, you can compare the various versions of methods in the file with their in-memory counterpart, filter them according to several criteria, load them as desired, etc.
