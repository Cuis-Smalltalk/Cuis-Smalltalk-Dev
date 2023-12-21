'From Cuis6.0 [latest update: #6144] on 20 December 2023 at 3:42:47 pm'!

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 12/20/2023 15:42:20'!
registerUpdate: changeSetOrPackageName

	| prefix |
	prefix := SystemVersion current updatesPrefix.
	(changeSetOrPackageName beginsWith: prefix) ifTrue: [
		(changeSetOrPackageName integerOrNilAt: prefix size+1)
			ifNotNil: [ :number |
				highestUpdate := self highestUpdate max: number.
				lastUpdateDate := Date today ]].! !

