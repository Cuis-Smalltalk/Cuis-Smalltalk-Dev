'From Cuis 4.2 of 25 July 2013 [latest update: #2486] on 12 September 2015 at 11:06:10.803397 pm'!

!FmFileEntry class methodsFor: 'system startup' stamp: 'jmv 9/12/2015 23:04'!
shutDown
	FmFileIOAccessor initialize! !

!FmFileEntry class methodsFor: 'system startup' stamp: 'jmv 9/12/2015 23:03'!
startUp
	self allSubInstancesDo: [:each | each refresh]! !


!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'jmv 9/12/2015 23:04'!
initialize
	"
	FmFileIOAccessor initialize
	"
	Default := nil! !

!methodRemoval: FmFileIOAccessor class #beDefault!
FmFileIOAccessor class removeSelector: #beDefault!
!methodRemoval: FmFileIOAccessor class #initDefault!
FmFileIOAccessor class removeSelector: #initDefault!
!methodRemoval: FmFileIOAccessor class #onSystemShutDown!
FmFileIOAccessor class removeSelector: #onSystemShutDown!
!methodRemoval: FmFileIOAccessor class #onSystemStartUp!
FmFileIOAccessor class removeSelector: #onSystemStartUp!
FmFileIOAccessor initialize!
!methodRemoval: FmFileEntry class #onSystemStartUp!
FmFileEntry class removeSelector: #onSystemStartUp!

!FmFileEntry class reorganize!
('accessing' drives)
('instance creation' entry: pathComponents: pathComponents:drive: pathName:)
('examples' example1 example2 example3 example4 example5 example6 example7)
('system startup' shutDown startUp)
!

