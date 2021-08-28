'From Cuis 5.0 [latest update: #4812] on 28 August 2021 at 7:16:28 pm'!

!ObjectExplorerWrapper methodsFor: 'monitoring' stamp: 'jmv 8/28/2021 19:13:11'!
refresh
	"Refresh item given an object and a string that is either an index or an instance variable name."
	| index |
	(model class allInstVarNames includes: itemName)
		ifTrue: [
			item _ model instVarNamed: itemName ]
		ifFalse: [
			item _ nil.
			index _ itemName findPositiveInteger.
			(index notNil and: [index between: 1 and: model basicSize]) ifTrue: [
				item _ model basicAt: index]]! !

