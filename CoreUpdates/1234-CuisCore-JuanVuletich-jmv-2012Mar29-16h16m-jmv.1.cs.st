'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 29 March 2012 at 4:18:23 pm'!

!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 3/29/2012 16:17'!
                      browse

	| current |
	current _ model selection.
	current ifNil: [ ^self ].! !

!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 3/29/2012 16:16'!
    deletePackage

	| current cs |
	current _ model selection.
	current ifNil: [ ^self ].
	model selectionIndex: 0.	"no selection"
	cs _ ChangeSet changeSetForPackage: current.
	cs isEmpty ifFalse: [
		cs name: cs hash asString, cs name ].
	CodePackage deregister: current! !

!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 3/29/2012 16:17'!
   diffs
	| current |
	current _ model selection.
	current ifNil: [ ^self ].! !

