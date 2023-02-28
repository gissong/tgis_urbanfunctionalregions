import math

#read the categories file that only including the within certain level of nodes
f=open('secondlevel_categories20170519.csv','r')
targetcategories=set()
while True:
    line=f.readline().rstrip()
    if len(line)==0:
        break
    targetcategories.add(line)
f.close()
print(len(targetcategories))

#read the category parent node dictionary
f=open('FSCatNameParentDict20170519.csv','r',encoding='UTF8')
parentNameDict=dict()
while True:
    line=f.readline().rstrip()
    if len(line)==0:
        break
    data=line.split(':')
    parentNameDict[data[0]]=data[1]
f.close()
print(len(parentNameDict))

def getTargetCategory(catstr):
    #print('function getTargetCategory called:'+str(catstr))
    global _category
    if catstr in targetcategories:
        _category = catstr
    else:
        #print(catstr+' parent: '+parentNameDict[catstr])
        getTargetCategory(parentNameDict[catstr])

dict_distances=dict()
dict_distances['chicago']=7389
dict_distances['dallas']=8994
dict_distances['houston']=8647
dict_distances['la']=4474
dict_distances['ny']=7123
dict_distances['pa']=8894
dict_distances['phoenix']=7969
dict_distances['sanantonio']=7475
dict_distances['sd']=7837
dict_distances['sanjose']=4822
citynameArray=['chicago','dallas','houston','la','ny','pa','phoenix','sanantonio','sd','sanjose']

for cityname in citynameArray:
    path='secondlevel_200pts_dist95_popularityln/'
    fr=open(cityname+'_fsqvenues200.txt','r')
    fr.readline()#headline
    flag=1
    while True:
    #for i in range(2):
        line=fr.readline()
        if len(line)==0:
            break
        data=line.split('\t')
        cityid=int(data[0])
        if cityid==flag:
            fw=open(path+cityname+str(cityid)+'.txt','a')
        else:
            fw.close()
            flag=cityid
            fw=open(path+cityname+str(cityid)+'.txt','a')
        num_users=int(data[10])
        getTargetCategory(data[8])
        #print(cityname+' targetCategory: '+str(_category))
        categories=_category.lower().split(' ')
        categoryStr=''
        categoryStr='-'.join(categories)
        distance=float(data[7])
        #print categoryStr
        if distance < dict_distances[cityname] and num_users > 1:
            if num_users > 1:
                freq_category = int(math.ceil(math.log(num_users)))
            else:
                freq_category = 1
            for i in range(freq_category):
                fw.write(categoryStr+'\n')#category only
                #fw.write(data[3]+'\t'+data[8]+'\n')#name and category
    fr.close()
    fw.close()

print('END')


    
    
