# Using Git and GitHub to host and manage Cuis code

Cuis includes tools and procedures for managing Smalltalk code. Central to this is the management of Packages and Package Files (.pck), and ChangeSet and ChangeSet files (.cs.st). But Cuis doesn't do version control by itself. Instead, we suggest using external VCS tools.  The Cuis project is hosted on [GitHub](http://www.github.com/), as most projects related to Cuis.

The guiding principle is to *not duplicate concepts and behavior*. We use GitHub to host, version, diff and merge external packages (.pck files), i.e. code that is maintained independently and outside Cuis.

Package files need to be simple text files. Cuis code files are encoded in UTF-8, and use the LF (ascii code 10) newline convention. This allows Git/GitHub to diff versions, and merge branches.

## Using Git/GitHub to host External Packages

There is a additional information on External Packages and how to use them at [Managing your code in Cuis](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/CodeManagementInCuis.md). Please take a look. What follows is the suggested procedure for using Git/GitHub to host external packages, and store their version history. Usually do this every day.

* Start with a standard (i.e. fresh) Cuis image. Never save the image.

* Set up Git repositories for external packages (if not already done)

* Install packages from Git repositories.

* Develop. Modify and/or create packages.

* Save own packages (to Git repositories).

* Git add / commit / push as appropriate.

* Save changes that are not part of any package. These are automatically captured in numbered ChangeSets, separated from changes to packages.

* Exit the image. Usually without saving.
