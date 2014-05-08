Cuis
----

[Cuis](http://www.cuis-smalltalk.org) is a free Smalltalk-80 environment originally derived from [Squeak](http://www.squeak.org) with a specific set of goals: being simple and powerful. It is also portable to any platform, fast and efficient. This means it is a great tool for running on any hardware, ranging from supercomputers to tablets and smart phones, and everything in between, including regular PCs.

Cuis is

* Easy to understand and extend
* Small
* Clean
 
Like Squeak, Pharo and other Squeak variants, Cuis is also:
* Open Source
* Multiplatform

Like other Smalltalk-80 environments (including Squeak, Pharo and others), Cuis is also:
* A complete development environment written in itself
* A pure Object Oriented language


Cuis is different from other Squeak variants in that it takes an active attitude towards system complexity.

As systems grow older, they usually become more complex. New features are added as layers on top of whatever is below, sometimes without really understanding it, and almost always without modifying it. Complexity grows without control. At some point, the system can't evolve anymore and becomes "legacy code".

The only way to avoid this is by understanding the complete system, and reengineering the whole system all the time. Keeping it simple and consistent.

This is important. Complexity puts a limit to the level of understanding of the system a person might reach, and therefore limits the things that can be done with it. Dan Ingalls says all this in ["Design Principles Behind Smalltalk"](http://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html). Even if you have already done so, please go and read it again!

Cuis is continuously evolving towards simplicity. Each release is better (i.e. simpler) than the previous one. At the same time, features are enhanced, and any bugs fixed. Cuis includes recent enhancements from Squeak, but only those that meet Cuis objectives: stuff the complexity of which outweighs its utility is not included.

If you want to know how Cuis evolved please check the [Release Notes](http://www.cuis-smalltalk.org/CuisReleaseNotes.html).

### Getting Started ###
To get started with Cuis you need to do the following:

1. Download Cuis by clicking on [Download ZIP](https://github.com/bpieber/Cuis-Smalltalk-Dev/archive/master.zip).

2. Extract the downloaded ZIP archive.

3. Get a VM for your platform. You can run Cuis with Squeak's official Virtual Machines from http://www.squeakvm.org or the faster Cog VMs from [Eliot Miranda's site](http://www.mirandabanda.org/files/Cog/VM/). (Please use the non-MT variants of Eliot's latest release.)

4. Start Cuis by dragging the Cuis 4.2 image onto the VM.

As an alternative you can use the following in a shell if you have Git installed and configured:
```
mkdir CuisDevelopment
cd CuisDevelopment
git clone https://github.com/bpieber/Cuis-Smalltalk-Dev.git
Cuis-Smalltalk-Dev/bin/newImage.sh MyProject
```
The last line creates a new image with a current Cog VM in a folder named MyProject. Start Cuis by dragging MyProject.image onto the Cog VM. This method has the advantage to set you up to easily contribute code back to Cuis packages.

Disclaimer: It should work on OS X and Unix. However, I only tested it on OS X. Please test on other platforms and let me know the results. Feel free to send pull requests!

### Contributing to Cuis ###
Cuis is maintained on https://github.com/Cuis-Smalltalk.

Please read [Code Management in Cuis 4](http://www.cuis-smalltalk.org/CodeManagementInCuis4.html), about developing packages for Cuis, and [Using Git and GitHub to host and manage Cuis code](http://www.cuis-smalltalk.org/CuisAndGitHub.html). While Cuis should work equally well with any file-based DVCS, we encourage the use of Git and GitHub.

To contribute code, please use an image with all included packages already loaded, and include new versions of affected packages. This will ensure we don't break them while we evolve Cuis.
Here is a Smalltalk script to load all packages currently included:
```
#('Graphics-Files-Additional' 'Network-Kernel' 'SignalProcessing' 'Tests' 'Theme-Themes' 'FFITests' 'JSON')
    do: [:each | Feature require: each]
```

Cuis is distributed subject to the MIT License. See the LICENSE file. Any contribution submitted for incorporation into or for distribution with Cuis shall be presumed subject to the same license.


# Code Management in Cuis 4.0

Cuis 4 includes new tools and new suggested procedures for managing Smalltalk code. Code that is not part of the Cuis Core image itself, like applications, frameworks and libraries, should be stored in Packages. New code that is meant as patches, fixes or additions; that could eventually become part of Cuis itself, is not part of any Package, and is therefore automatically stored in Change Sets.

## Packages

Let's start with Packages. The Package implementation in Cuis 4 is based on PackageInfo, the standard way to specify packages in Squeak and its derivatives, and used, for example, by Monticello. It uses Package names, to specify prefixes for Class and Method categories. Classes and Methods whose categories match a Package's prefixes belong in that Package. More details about how PackageInfo decides what code belongs in a package are available at http://wiki.squeak.org/squeak/3329 .

To install packages (.pck files) in Cuis, use the FileList, navigate to the appropriate directory (on disk, or in a GitHub repository, etc), select the package file and click on [Install Package].

Cuis includes a tool to manage installed Packages. It is at World / Open / Installed Packages. To create a new package (instead of installing an existing one from a file), click on [Create Package] This creates a new package, and associates with it all the existing code in the image that matches the package name.

The operations available on installed or newly created packages are:

[Save] Saves a package on the file system. Overwrites any existing version. It is good to save package from time to time, to reduce the risk of losing code.

[Delete] Removes the Package instance from the image. Does not remove any code. This means, effectively, to merge back the code into Cuis.

[Browse unsaved Changes] This opens a ChangeSorter on the ChangeSet that captures all the changes done to the Package since it was last saved. Therefore it shows the work done on the package that would be lost if the package is not saved.

[Browse Package Code] This opens a Class Browser that only shows the code that belongs in the package. This is useful for working on a package, or studying it.

The tool shows, for each Package, the name, whether it is dirty (has unsaved changes) and the file it was installed from / saved to.

Handling Packages like this, Cuis behaves as a sort of document editor (like, for example a regular text editor) whose documents are Package files (.pck). Cuis doesn't handle Package versions, ancestries, etc. If versioning of Packages is desired, the best is to use a versioning file repository, such as Git or Mercurial. The recommendation is to use a GitHub repository with a name beginning with 'Cuis-', so it will be easy for anybody to find it. Cuis Package files (.pck) are uncompressed, use Lf (ASCII 10) as newLine, and are encoded in ISO 8859-15. This means that are Git friendly, and Git/GitHub can diff and merge them, and browse them with syntax highlighting.

This is not unlike using Git or GitHub with a more conventional development environment such as Eclipse or a text editor. Like Cuis 4, these tools don't do version handling themselves, they just load and save files; and let Git do its magic.

## Changes to the Cuis base image

The way ChangeSets are created and managed in Cuis 4 is very different from previous versions of Cuis (and Squeak & derivatives). This was done to make ChangeSets a good way to manage changes to the base Cuis Core image, while keeping code in Pakges out of the way, so they don't get mixed together.

What is not in a Package belongs (at least temporarily) in the Cuis Core image. Such code is automatically captured in a ChangeSet. The ChangeSet for Core changes is created automatically and named like "1243-CuisCore-JuanVuletich-2012Apr03-22h50m". The number at the beginning is the next number for the Cuis update stream, and is provided only as a suggestion. The "CuisCore" part is to reveal that the code belongs in the base image and not in some package. Then we have author name and date / time of creation. These ChangeSets are created automatically. There is no longer a way to manually create them, or make them "current" or "active". It is best not to rename them. These ChangeSets will not capture any code that belongs in a Package.

Opening a Change Sorter will show the CuisCore change set. This is useful, for example, to check that no code that was intended for a Package ends here by mistake (because of the wrong class or method category). But it is also useful when doing changes to the base system. Now, we can do changes both to the base system and to a number of packages, all in the same session, without having to be careful about selecting the proper change set before saving a method: The code is automatically added to the proper Package or ChangeSet, simply following the class or method category. Gone are the days of messed up change sets and lost code!

When the changes to the base system are complete, it is a good time to review the CuisCore change set and, maybe remove from it changes that we don't want to keep (for example, experiments, halts, etc). Then, just do right click / File out and remove. This saves the ChangeSet on disk. It also removes it from the ChangeSorter (but it doesn't remove any code). This is good, because the next changes done will end in a new CuisCore change set, and there's no risk of having undesired changes in the old one. As changes to the base image progress, and several CuisCore change sets are saved to disk, these numbered files are created in sequence. They will be ready to be loaded back in proper order in a fresh Cuis image, or to be sent to Cuis maintainers for integration in the update stream and in next releases of Cuis.

## Loading ChangeSet files into Cuis

There are two ways to load ChangeSet files (.cs): [FileIn] and [Install].

[FileIn] loads the code without creating a new ChangeSet object. This means that changes that belong in the base image (and not in a package) will be added to the current ChangeSet for Cuis core changes, as if they were done by the user. This is appropriate when we are combining code from more than one source into a single ChangeSet. Any change that belongs in an installed package will be added to it, and the package will appear as dirty.

[Install] loads the code into a separate ChangeSet object (viewable in the ChangeSorter tool). This is appropriate for loading Cuis updates, or other code that we are not authoring, as it doesn't add new items (class or method definitions) to the current ChangeSet for our changes to Cuis. Usually any ChangeSets should be installed before doing changes to the image. The reason is that an installed ChangeSet could overwrite changes done by you, or packages you have installed. If this is the case, the affected packages would appear as dirty, and your change set would include any installed changes (that don't belong in a package). Be careful when saving packages or change sets if this was the case!
