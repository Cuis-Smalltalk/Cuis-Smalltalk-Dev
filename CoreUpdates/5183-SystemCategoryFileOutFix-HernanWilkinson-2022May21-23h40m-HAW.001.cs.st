'From Cuis 6.0 [latest update: #5173] on 22 May 2022 at 12:52:35 am'!

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'HAW 5/22/2022 00:50:26'!
fileOutCategory: aCategoryRoot
	"FileOut all the classes in the named system category."

	DirectoryEntry smalltalkImageDirectory // (aCategoryRoot asFileName , '.st') writeStreamDo: [ :fileStream |
		(self categoriesMatching: aCategoryRoot,'*') do: [ :category | 
			self fileOutCategory: category on: fileStream initializing: true ]]! !

