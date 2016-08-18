'From Cuis 4.2 of 25 July 2013 [latest update: #2632] on 19 December 2015 at 3:06:56.383959 pm'!
!classDefinition: #Encoder category: #'Compiler-Kernel'!
ParseNode subclass: #Encoder
	instanceVariableNames: 'scopeTable nTemps supered requestor class selector literalStream selectorSet litIndSet litSet sourceRanges globalSourceRanges addedSelectorAndMethodClassLiterals optimizedSelectors cue '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Class methodsFor: 'accessing' stamp: 'jmv 12/19/2015 12:36'!
classPool: aDictionary
	classPool := aDictionary! !


!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 12/19/2015 12:41'!
methodClassAssociation: aBinding
	"sets the association to the class that I am installed in"
	^self literalAt: self numLiterals put: aBinding! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 12/19/2015 15:05'!
renameClass: aClass from: oldName to: newName
	"Rename the class, aClass, to have the title newName."
	| oldref i category |
	category := SystemOrganization categoryOfElement: oldName.
	SystemOrganization classify: newName under: category.
	SystemOrganization removeElement: oldName.
	oldref _ self associationAt: oldName.
	self removeKey: oldName.
	oldref key: newName.
	self add: oldref.  "Old association preserves old refs"
	(Array with: StartUpList with: ShutDownList) do:
		[:list |  i _ list indexOf: aClass name ifAbsent: [0].
		i > 0 ifTrue: [list at: i put: newName]].
	self flushClassNameCache.
	SystemChangeNotifier uniqueInstance classRenamed: aClass from: oldName to: newName inCategory: category! !


!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 12/19/2015 12:49'!
flush
	self endEntry! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 12/19/2015 15:05'!
renameClass: aClass as: newName 
	^self renameClass: aClass from: aClass name to: newName! !

!classDefinition: #Encoder category: #'Compiler-Kernel'!
ParseNode subclass: #Encoder
	instanceVariableNames: 'scopeTable nTemps supered requestor class selector literalStream selectorSet litIndSet litSet sourceRanges globalSourceRanges addedSelectorAndMethodClassLiterals optimizedSelectors cue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
