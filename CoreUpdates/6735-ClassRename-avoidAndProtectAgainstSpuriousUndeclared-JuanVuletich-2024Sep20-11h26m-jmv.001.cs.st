'From Cuis7.1 [latest update: #6734] on 20 September 2024 at 11:27:18 am'!

!Class methodsFor: 'class name' stamp: 'jmv 9/20/2024 11:11:38'!
rename: aString
	"The new name of the receiver is the argument, aString."

	| newName |
	(newName := aString asSymbol) ~= self name
		ifFalse: [^ self].
	(Smalltalk includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists'].
	(Undeclared includesKey: newName)
		ifTrue: [
			Undeclared removeUnreferencedKeys.
			(Undeclared includesKey: newName)
				ifTrue: [
					self inform: 'There are references to, ' , aString printString , '
from Undeclared. Check them after this change.']].

	self safeRenameTo: newName.! !


!RenameObject methodsFor: 'applying - private' stamp: 'jmv 9/20/2024 11:25:07'!
renameReferences

	| references |

	references := self referencesToRename.
	references := self rejectReferencesToClassVariablesFrom: references.
	references do: [ :aReference | self renameReference: aReference compiledMethod ].
	"See #newSourceCodeOf: . These spurious undeclared entries may be a problem later."
	Smalltalk cleanOutUndeclared.
	^references! !

