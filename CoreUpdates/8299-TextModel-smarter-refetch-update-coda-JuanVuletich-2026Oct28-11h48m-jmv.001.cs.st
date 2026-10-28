'From Cuis7.9 [latest update: #8296] on 28 October 2026 at 11:49:14 am'!

!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 28/Oct/2026, 2:34:42 pm (UTC)'!
updateIfNeeded
	self acceptedContentsChanged.! !

!methodRemoval: CodeFileBrowser #didCodeChangeElsewhere stamp: 'jmv 28/Oct/2026, 2:48:30 pm (UTC)'!
CodeFileBrowser removeSelector: #didCodeChangeElsewhere!
!methodRemoval: CodeProvider #didCodeChangeElsewhere stamp: 'jmv 28/Oct/2026, 2:48:30 pm (UTC)'!
CodeProvider removeSelector: #didCodeChangeElsewhere!
