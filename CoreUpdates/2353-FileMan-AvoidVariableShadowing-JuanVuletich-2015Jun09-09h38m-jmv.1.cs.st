'From Cuis 4.2 of 25 July 2013 [latest update: #2354] on 9 June 2015 at 9:38:54.911197 am'!

!FmFileIOAccessor class methodsFor: 'accessing' stamp: 'jmv 6/9/2015 09:38'!
dialectUtilClass
	^ dialectUtilClass
		ifNil: [| subclss |
			subclss := FmPortableUtil subclasses.
			subclss notEmpty
				ifTrue: [dialectUtilClass := subclss at: 1]]! !

!FmFileIOAccessor class methodsFor: 'accessing' stamp: 'jmv 6/9/2015 09:38'!
librarySpecificClass
	^ librarySpecificClass
		ifNil: [| subclss |
			subclss := self subclasses.
			subclss notEmpty
				ifTrue: [librarySpecificClass := subclss at: 1]]! !

