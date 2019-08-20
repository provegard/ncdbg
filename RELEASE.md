# How to make a release

1. Update _CHANGELOG.md_ with the new version.
2. Create a commit with message "Release version X".
3. Tag the commit, `git tag X`.
4. Build the ZIP file, `gradlew distZip`.
5. Collect the build file in _build/distributions_.