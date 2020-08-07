Process to get your code up on the server:

1. `git init`   ( If the repo doesn’t already exist)
2. `git add .` (adds all the files to the repo)
3. `git commit -m "name"`  commits changes
4. `git remote add origin https://github.com/<username>/<reponame>.git` (if you haven’t set the URL yet)
5. `git push -u origin master`   pushes the code to the server

To remove cached files:

```git rm -r --cached . ```
