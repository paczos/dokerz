docker build --rm -t 42n4/rstudio .

https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image

https://github.com/rocker-org/rocker-versioned/tree/master/rstudio

#For linux with exact username and its user id and group id
docker run -d -p 8787:8787 -e USER=guest -e USERID=1001 -e GROUPID=100 -v $(pwd):/home/guest 42n4/rstudio

#For Windows Docker with Linux containers enabled and Powershell and shared disk c: in docker settings
docker run -d -p 8787:8787 -v c:/Users/Piotr/remote:/home/rstudio 42n4/rstudio
