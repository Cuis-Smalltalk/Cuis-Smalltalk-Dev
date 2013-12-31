'From Cuis 4.0 of 21 April 2012 [latest update: #1267] on 30 April 2012 at 12:10:30 am'!

!CodePackage commentStamp: 'jmv 4/30/2012 00:03' prior: 0!
A CodePackage is a package that is currently loaded in the system. If saved (.pck), then it is stored in a file that can be dealt with as an instance of PackageFile. As the code is already in the system, all we need to know is the packageName. Implementation is originally based on PackageInfo, but has diverged.

CodePackage instances are usually created when installing CodePackageFiles. These instances track the code for that package, that we'll need to save if we don't want to lose changes. These instances are held in the InstalledPackages class variable.

We can also create 'transient' instances with whatever name (and classes and extension methods) we chose, like
	(CodePackage named: 'Collections' createIfAbsent: true registerIfNew: false) inspect; save
This won't mean the system is actually partitioned in such way.

(CodePackage named: 'TestPackage' createIfAbsent: true registerIfNew: false) inspect; save!

