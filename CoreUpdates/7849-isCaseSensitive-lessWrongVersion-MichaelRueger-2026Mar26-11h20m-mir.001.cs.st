'From Cuis7.7 [latest update: #7848] on 26 March 2026 at 11:22:35 am'!

!FileIOAccessor methodsFor: 'testing' stamp: 'mir 17/Mar/2026 18:47:32'!
isCaseSensitive
	"
	FileIOAccessor default isCaseSensitive
	Note: This is the wrong question to ask. See
		https://en.wikipedia.org/wiki/Case_sensitivity ('In filesystems' section).
		https://en.wikipedia.org/wiki/Case_preservation
	"
	^self onUnix and: [(Smalltalk platformName = 'Mac OS') not]! !

