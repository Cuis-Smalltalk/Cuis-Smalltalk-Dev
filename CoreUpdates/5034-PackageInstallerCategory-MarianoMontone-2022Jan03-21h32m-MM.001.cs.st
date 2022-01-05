'From Cuis 6.0 [latest update: #5032] on 3 January 2022 at 8:33:44 pm'!
!classDefinition: #PackageInstallerWindow category: #'Tools-Packages'!
SystemWindow subclass: #PackageInstallerWindow
	instanceVariableNames: 'filterString filterInput currentIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Packages'!
!classDefinition: #PackageInstaller category: #'Tools-Packages'!
Object subclass: #PackageInstaller
	instanceVariableNames: 'packageList'
	classVariableNames: 'PackageList'
	poolDictionaries: ''
	category: 'Tools-Packages'!
