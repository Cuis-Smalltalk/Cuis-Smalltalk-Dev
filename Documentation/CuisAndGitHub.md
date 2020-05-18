# Using Git and GitHub to host and manage Cuis code

Cuis includes tools and procedures for managing Smalltalk code. Central to this is the management of Packages and Package Files (.pck). But Cuis doesn't do version control. Instead, we suggest using external VCS tools.  In particular, we're using [GitHub](http://www.github.com/), and the first project we're hosting there is [StyledTextEditor](https://github.com/bpieber/Cuis-StyledTextEditor).

The guiding principle is to *not duplicate concepts and behavior*. As we're using an external tool (Git) for version control, then we use it as it meant to be used. Most people use Git for version control and a file based IDE such as Eclipse for development. Such IDEs don't do version control themselves. It is done by Git. Do the same: do not include package version control in Cuis. This is a departure from the Monticello /Git integration (smallsource and MonticelloFileTree) by Otto Behrens, Dale Henrichs, etc.

We use GitHub to host, version, diff and merge external packages (.pck files), i.e. code that is maintained independently and outside Cuis.

Package files need to be simple text files. Cuis encoding for latin alphabet (ISO 8859-15) is handled without problems by GitHub. Cuis uses the LF (ascii code 10) newline convention, as preferred in GitHub. This allows Git/GitHub to diff versions, and merge branches.

Each GitHub repository has one set of users and permissions. Each GitHub repository has one state (Git commits repositories, not individual files). Branch and merges are done on the whole repository and not on individual files. Therefore, we need a separate GitHub repository for each project, i.e., for each package or set of closely related packages that are always loaded and maintained together as a whole.

## Using Git/GitHub to host External Packages

There is a additional information on External Packages and how to use them at [Managing your code in Cuis](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/CodeManagementInCuis.md). Please take a look. What follows is the suggested procedure for using Git/GitHub to host external packages, and store their version history. Usually do this every day.

* Start with a standard (i.e. fresh) Cuis image. Never save the image.

* Set up Git repositories for external packages (if not already done)

* Install packages from Git repositories.

* Develop. Modify and/or create packages.

* Save own packages (to Git repositories).

* Git add / commit / push as appropriate.

* Fileout changes that are not part of any package. These are automatically captured in numbered changesets, separated from changes to packages.

* Exit the image. Usually without saving.
