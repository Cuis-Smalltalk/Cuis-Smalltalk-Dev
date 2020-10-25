'From Cuis 5.0 [latest update: #4426] on 25 October 2020 at 8:34:32 pm'!

!CodePackage methodsFor: 'accessing' stamp: 'jmv 10/25/2020 20:33:45'!
codePackageClass
	"Answer the specific CodePackage subclass to use."

	self class == CodePackage ifFalse: [
		^ self class ].
	self classesDo: [ :cls |
		(cls inheritsFrom: CodePackage)
			ifTrue: [
				((self packageName asIdentifier: true), 'Package') = cls name ifTrue: [
					^ cls ]]].
	^ nil! !

