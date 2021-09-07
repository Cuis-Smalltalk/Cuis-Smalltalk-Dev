'From Cuis 5.0 [latest update: #4851] on 7 September 2021 at 12:04:13 pm'!

!CodeWindow methodsFor: 'updating' stamp: 'jmv 9/7/2021 12:02:33'!
updateListsAndCode
	"All code windows receive this message on any code change in the system.
	Process it only once, for the benefit of installing large packages!!"

	(self hasProperty: #updateListsAndCode) ifFalse: [
		self setProperty: #updateListsAndCode toValue: true.
		self whenUIinSafeState: [
			self removeProperty: #updateListsAndCode.
			owner ifNotNil: [ self updateListsAndCodeNow ]]].! !

