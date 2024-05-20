'From Cuis7.1 [latest update: #6379] on 20 May 2024 at 10:58:25 am'!

!ProgressiveTestRunner methodsFor: 'show result - private' stamp: 'jmv 5/20/2024 10:57:59'!
showDefects
	
	 | defects |
			
	defects := testResult defects.
	"defects size = 1" false
		ifTrue: [ defects anyOne debug ] 
		ifFalse: [ self openTestResultWindow]! !

