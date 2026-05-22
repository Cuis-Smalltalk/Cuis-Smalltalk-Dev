Class {
	#name : 'ClassCommentReader',
	#superclass : 'ClassCategoryReader',
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'fileIn/Out' }
ClassCommentReader >> scanFrom: aStream  [
	"File in the class comment from aStream.  Not string-i-fied, just a text, exactly as it is in the browser.  Move to changes file."

	class theNonMetaClass classComment: aStream nextChunk stamp: changeStamp
		"Writes it on the disk and saves a RemoteString ref"
]
