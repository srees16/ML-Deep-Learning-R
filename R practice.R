#Derek Banas Tutorial
print(class(53))
print(class(5L))
print(class("Hi"))
print(class(4i))
print(class(T))
print(class(charToRaw("24")))
a = 34
as.character(a)
c = "hello"
is.character(c)
is.integer(a)
is.numeric(a)
is.element(c)
is.object(c)
is.nan(a)

number=c(6,7,2,4,8,3,5)
number[2]
length(number)
number[length(number)]
number[-2]
number[c(1,2)]
number[c(5,6)]
number[2:5]
number[2]=9
number
number[c(3,4)]=c(8,4)
sort(number)
number
number[3]=1
sort(number,decreasing = T)
sort(number,decreasing = F)
mySeq=1:8
Ad=seq(from=2,to=9,by=2)
Ad
even=seq(from=0,by=2,length.out = 10)
even
4 %in% even
3 %in% even
rep(x=2,times=3)
rep(x=c(4,5), times=2,each=2)
one2ten=c(1:10)
isEven=one2ten %% 2==0
isEven
onlyEvens=one2ten[one2ten%%2==0]
onlyEvens
cat(T&&F)
cat(T&&T)
cat(F&&T)
cat(F&&F)

grade="B"
switch(grade,
       "A"=print("Great"),
       "B"=print("Good"),
       "C"=print("OK"),
       print("We are sorry!"))

Str1="This is my first string in sentence"
nchar(Str1)
"dog"<"elf"
"dog"=="elf"
str2=paste("Bear","grylls")
str2
str3=substr(x=str2,start=3,stop=9)
str3
ss=sub(pattern="Bear",replacement = "SAS",x=str2)
ss
gsub(pattern="SAS",replacement = "Tiger",x=ss)
stringpit=strsplit("A boy on the road"," ")
stringpit
mst="Hello everyone"
strsplit(mst," ")
directions=c("up","down","left","right","center","up","left")
ff=factor(directions)
ff
is.factor(ff)
levels(x=ff)
dow=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
wdays=c("Wednesday","Friday","Saturday")
wFactors=factor(x=wdays,levels = dow,ordered=T)
wFactors

names=c("jack","ass","jill")
ages=c(24,13,19)
rank=c(2,3,1)
gender=c("male","female","male","male","female","male")
custData=data.frame(names,gender,ages,rank,stringsAsFactors = F)
custData
custData[2,1]
custData[3,2]
custData[1,1:2]
custData[1,1:3]
custData[1:3,2]
dim(custData)
addRecord=data.frame(names="hill",ages=23,rank=4)
custData=rbind(custData,addRecord)
custData
add1more=data.frame(rank=2,names="bill",ages=21)
custData=rbind(custData,add1more)
custData
experience=c(4,5,3,4,6,2)
custData=cbind(custData,experience)
custData
suitableage=custData[custData$ages>13,]
suitableage
custData

no=1
repeat {
  print(no)
  no=no+1
  if(no>5) {
    break
  }
}
no=5
while(no>0) {
  no=no-1
  if(no%%2==0) {
    next
  }
  print(no)
}

#range=1:5
for(i in 1:5) {
  print(i)
}
mrow=c(5,3,6,7,4,1)
matrix1=matrix(mrow,nrow=2,ncol=3,byrow=T)
matrix1

dim(matrix1)
matrix1[2,3]
matrix1[2,]
matrix1[,3]
mtrix=rbind(1:3,4:6,7:9)
mtrix
mtrix[1:2,]
mtrix[1:3]
mtrix[2:2]
mtrix[3:3]=-2
mtrix
mtrix[3,3]=-3
mtrix
mtrix[2,2]=-5
mtrix
mtrix[1,]=c(-3,-1,-8)
mtrix
arr1=array(c(1:9),dim = c(2,2,2))
arr1
arr1[2,2,1]

getsum=function(s1=12,s2=-10) {
  s1+s2
}
getsum()

words=function(astring) {
  return (strsplit(astring," "))
}
words("Kiss my ass")

missingGuy=function(x) {
  if(missing(x)) {
    return ("Missing args")
  } else {
    return (x)
  }
}
missingGuy("hello ji")

getMore=function(...) {
  numlist=list(...)
  sum=0
  for(i in numlist) {
    sum=sum+i
  }
  sum
}
getMore(20,20,40,60,10)

numblist=1:10
doubling=(numblist)(function(x)x*2)
doubling

roots=function(x)sqrt(x%*%x)
roots(1:3)

numlist=1:5
doubling=(function(x)x*2)(numlist)
doubling

powr=function(exp) {
  function(x) {
    x^exp
  }
}
cubed=powr(3)
cubed(2)
#cubed(1:5)

mypower=function(exp) {
  function(x) {
    exp^x
  }
}
sqd=mypower(4)
sqd(2)

addFunc=list(
  add1=function(x)x+3,
  add2=function(y)x^2
)
addFunc$add1(4)

#Exceptions
div=function(n1,n2) {
  tryCatch(
    n1/n2,
    error=function(e) {
      if(is.character(n1) || is.character(n2)) {
        print("Invalid Operation idiot")
      }
    }
  )
}
div(10,"3")

myppl=read.table(file = file.choose(),
                 header=T,
                 sep = " ",
                 na.strings = "'",
                 stringsAsFactors = F)
myppl
addone=data.frame(fname="Jack",lname="ass",sex="T")
myppl=rbind(myppl,addone)
write.table(x=myppl,file=file.choose(),sep = " ",na="'",quote=F,row.names = F)
myppl
addtwo=data.frame(fname="Daniels",lname="Jack",sex="M")
myppl=rbind(myppl,addtwo)
write.table(x=myppl,file=file.choose(),quote = F,row.names = F)
myppl

head(myppl,4)
tail(myppl)
d1=c(4,5,3,5,7)
d2=c(3,2,5,3,1)
xyplot=matrix(d1,d2,nrow=5,ncol=2)
plot(xyplot)

d3=c(1,2,3,4,5)
d4=c(1,2,3,4,5)
plot(d3,d4,type="o",main="Test Plot",xlab="X AXIS",ylab = "Y AXIS",col="rosybrown")

ceiling(46.65)
floor(54.54)
Rng=c(3,46,3,6,7443,6)
min(Rng)
max(Rng)
range(Rng)
sum(5,3,7,2)
cumprod(c(1,2,3,4,5,6))

sample(0:100,10,replace=T)
