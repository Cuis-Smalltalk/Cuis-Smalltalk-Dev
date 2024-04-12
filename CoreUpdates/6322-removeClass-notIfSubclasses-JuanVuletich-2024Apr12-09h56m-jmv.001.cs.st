'From Cuis6.3 [latest update: #6321] on 12 April 2024 at 9:57:07 am'!

!CodeProvider methodsFor: 'commands' stamp: 'jmv 4/12/2024 09:54:41'!
removeClass
	"Remove the selected class from the system, at interactive user request.  Make certain the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| message  className classToRemove result |
	classToRemove := self selectedClassOrMetaClass ifNil: [Smalltalk beep. ^ false].
	classToRemove := classToRemove theNonMetaClass.
	className := classToRemove name.
	classToRemove subclasses notEmpty ifTrue: [
		self inform:
className, ' has subclasses.
Please remove them first.'.
			^ false ].
	message :=
'Are you certain that you
want to REMOVE the class ', className, '
from the system?'.
	(result := self confirm: message)
		ifTrue: [
			classToRemove removeFromSystem.
			self changed: #classList.
			true ].
	^ result.! !

