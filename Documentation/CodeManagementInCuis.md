# Managing your code in Cuis

Cuis includes tools and procedures for managing Smalltalk code. Code that is not part of the Cuis Core image itself, like applications, frameworks and libraries, should be stored in *Packages*. New code that are meant as patches, fixes or additions; that could eventually become part of Cuis itself, is not part of any *Package*, and is therefore automatically stored in *ChangeSets*.

## Packages

Let's start with *Packages*. The Package implementation in Cuis is based on PackageInfo, the standard way to specify packages in Squeak and its derivatives, and used, for example, by Monticello. It uses Package names to specify prefixes for Class and Method categories. Classes and Methods whose categories match a Package's prefixes belong in that Package. More details about how PackageInfo decides what code belongs in a package are available at http://wiki.squeak.org/squeak/3329 .

To install packages *(.pck.st files)* in Cuis, use the **FileList**, navigate to the appropriate directory (on disk, or in a GitHub repository, etc), select the package file and click on **[Install Package]**.

Cuis includes a tool to manage installed *Packages*. It is at *World / Open / Installed Packages*. To create a new package (instead of installing an existing one from a file), click on **[Create Package]** This creates a new package, and associates with it all the existing code in the image that matches the package name.

The operations available on installed or newly created packages are:

**[Save]** Saves a package on the file system. Overwrites any existing version. It is good to save the package from time to time, to reduce the risk of losing code.

**[Delete]** Removes the Package instance from the image. Does not remove any code. This means, effectively, to merge back the code into Cuis.

**[Browse unsaved Changes]** This opens a ChangeSorter on the ChangeSet that captures all the changes done to the Package since it was last saved. Therefore it shows the work done on the package that would be lost if the package is not saved.

**[Browse package code]** This opens a Class Browser that only shows the code that belongs in the package. This is useful for working on a package, or studying it.

**[Add requirement]** This opens a select list of loaded packages.  Each package provides a *Feature*.  You can CANCEL, require the current Cuis base version (at a minimum) or require any of the packages on the list.  Required packages will be loaded before the selected package (**Feature require: #'your-package'.**).  When a package is selected, the lower browser pane shows its requirents, which may be deleted.  Don't forget to *Save* your package after adding or deleting  requirements!

The tool shows, for each Package, the name, whether it is dirty (has unsaved changes) and the file it was installed from / saved to.

Handling Packages like this, Cuis behaves as a sort of document editor (like, for example a regular text editor) whose documents are *Package* files *(.pck.st)*. Cuis doesn't handle Package versions, ancestries, etc. If versioning of Packages is desired, the best is to use a versioning file repository, such as Git or Mercurial. The recommendation is to use a GitHub repository with a name beginning with 'Cuis-Smalltalk-', so it will be easy for anybody to find it. Cuis *Package* files are uncompressed, use Lf (ASCII 10) as newLine, and are encoded in ISO 8859-15. This means they are Git friendly, and Git/GitHub can diff and merge them, and browse them with syntax highlighting.

This is not unlike using Git or GitHub with a file-based development environment such as Eclipse or a text editor. Like Cuis, these tools don't do version handling themselves, they just load and save files; and let Git do its magic.

## Changes to the Cuis base image

The way *ChangeSets* are created and managed in Cuis is different from Squeak. This was done to make ChangeSets a good way to manage changes to the base Cuis Core image, while keeping code in Packages out of the way, so they don't get mixed together.

What is not in a Package belongs (at least temporarily) to the Cuis Core image. Such code is automatically captured in a *ChangeSet*. The ChangeSet for Core changes is created automatically and named like *"1243-CuisCore-JuanVuletich-2012Apr03-22h50m"*. The number at the beginning is the next number for the Cuis update stream, and is provided only as a suggestion. The "CuisCore" part is to reveal that the code belongs in the base image and not in some package. Then we have author name and date / time of creation. These *ChangeSets* are created automatically. There is no longer a way to manually create them, or make them "current" or "active". It is best to rename them, replacing *'CuisCore'* with some meaningful name. These *ChangeSets* will not capture any code that belongs in a Package.

Opening a **Change Sorter** will show the CuisCore change set. This is useful, for example, to check that no code that was intended for a Package ends here by mistake (because of the wrong class or method category). But it is also useful when doing changes to the base system. Now, we can do changes both to the base system and to a number of packages, all in the same session, without having to be careful about selecting the proper change set before saving a method: The code is automatically added to the proper *Package* or *ChangeSet*, simply following the class or method category. Gone are the days of messed up change sets and lost code!

When the changes to the base system are complete, it is a good time to review the CuisCore change set and, maybe remove from it changes that we don't want to keep (for example, experiments, halts, etc). Then, just do right click / File out and remove. This saves the *ChangeSet* on disk. It also removes it from the **ChangeSorter** (but it doesn't remove any code). This is good, because the next changes done will end in a new CuisCore change set, and there's no risk of having undesired changes in the old one. As changes to the base image progress, and several CuisCore *ChangeSets* are saved to disk, these numbered files are created in sequence. They will be ready to be loaded back in proper order in a fresh Cuis image, or to be sent to Cuis maintainers for integration in the update stream and in next releases of Cuis.

### Installing ChangeSet files into Cuis

**[Install]** loads all the code in the file into a separate, new *ChangeSet* object (viewable in the **ChangeSorter** tool). This is appropriate for loading Cuis updates, or other code that we are not authoring, as it doesn't add new items (class or method definitions) to the current ChangeSet used to record the changes we make to Cuis. Usually any ChangeSets should be installed before doing changes to the image. The reason is that an installed ChangeSet could overwrite changes done by you, or packages you have installed. If this is the case, the affected packages would appear as dirty, and your change set would include any installed changes (that don't belong in a package). Be careful when saving packages or change sets if this was the case!

## Cherry picking individual changes from ChangeSet or Package files

Additionally, you can study a Package (.pck.st) or ChangeSet (.cs) file without installing it. To do this, use the **FileList**, navigate to the appropriate directory, select the file and click on **[Contents]**. You will get a **ChangeList** tool with the contents of the file. You can select each change, to see the code, and compare it with what is currently loaded in the system (if that is the case). You can also various filters on the list. See the right-click menu. Once you have one or more changes selected, you can do right-click / 'fileIn selections'. Changes that belong in a package that is already there will be captured by that package, that will now be dirty. Code that doesn't belong in a loaded package will be included in the current *ChangeSet*, together with code you save in a **Browser**. A new *Package* or *ChangeSet* will *not* be created. This is especially useful when reviewing code, or when we are combining code from more than one source into a single *ChangeSet* or *Package*.
