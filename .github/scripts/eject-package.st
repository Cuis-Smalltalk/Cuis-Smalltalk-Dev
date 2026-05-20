"eject-package.st

Test gating is handled by eject-package.sh.

Args:
  1) package name
  2) mode: 'safe' | 'force'

Exports package sources in Tonel format under src/<PackageName>/.

In 'safe' mode: if any live instances of classes in the package exist, do nothing.
In 'force' mode: export anyway and then attempt to unload the package.
"

| pkgName mode outputDir package writer liveCounts hasLiveInstances |

Utilities classPool at: #AuthorName put: 'OpenCode'.
Utilities classPool at: #AuthorInitials put: 'OC'.

"Squeak/Cuis convention: command-line args start at system attribute 3."
pkgName := Smalltalk getSystemAttribute: 3.
mode := Smalltalk getSystemAttribute: 4.

(pkgName isNil or: [pkgName isEmpty]) ifTrue: [
  Transcript showCr: 'ERROR: missing package name argument'.
  Smalltalk quitPrimitive: 2
].

mode isNil ifTrue: [ mode := 'safe' ].

"Load the Tonel package writer if not already present"
(Smalltalk includesKey: #TonelWriter) ifFalse: [
  CodePackageFile installPackage:
    DirectoryEntry currentDirectory // 'Packages/System/Tonel.pck.st'.
].

package := CodePackage named: pkgName.
package isNil ifTrue: [
  Transcript showCr: 'ERROR: package not found: ', pkgName.
  Smalltalk quitPrimitive: 2
].

"Detect live instances for classes defined by this package"
liveCounts := OrderedCollection new.
package classes do: [ :cls |
  | count |
  count := cls allInstances size.
  count > 0 ifTrue: [ liveCounts add: { cls name. count } ].
].
hasLiveInstances := liveCounts notEmpty.

(mode = 'safe' and: [ hasLiveInstances ]) ifTrue: [
  Transcript showCr: 'ABORT: live instances exist for package ', pkgName, '.';
    showCr: 'Nothing was exported/unloaded. Instances:';
    yourself.
  liveCounts do: [ :pair |
    Transcript showCr: '  ', pair first, ' -> ', pair second asString
  ].
  Smalltalk quitPrimitive: 3
].

"Ensure src/<PackageName>/ exists"
(DirectoryEntry currentDirectory // 'src') exists ifFalse: [
  (DirectoryEntry currentDirectory // 'src') createDirectory
].
outputDir := DirectoryEntry currentDirectory // 'src' // pkgName.
outputDir exists ifFalse: [ outputDir createDirectory ].

writer := TonelWriter on: outputDir.
writer writePackage: package.

Transcript showCr: 'Exported Tonel package: ', pkgName, ' -> ', outputDir pathString.

hasLiveInstances ifTrue: [
  Transcript showCr: 'WARNING: live instances exist. Unloading may break the image.'
].

"Unload package from the image (safe mode only reaches here when no live instances)."
[ package unload ]
  on: Error
  do: [ :ex |
    Transcript showCr: 'ERROR: unload failed: ', ex description.
    Smalltalk quitPrimitive: 4
  ].
Transcript showCr: 'Unloaded package from image: ', pkgName.

"Persist the image without the ejected package."
Smalltalk snapshot: true andQuit: true.

Smalltalk quitPrimitive: 0
