#!/usr/bin/env python
# coding: utf-8

# In[2]:


pip install beautifulsoup4


# In[11]:


import requests
from bs4 import BeautifulSoup as bs


# In[13]:


source=requests.get("https://www.eenadu.net/").text


# In[22]:


soup=bs(source,'lxml')


# In[23]:


mainpage=soup.find('div')


# In[36]:


print(mainpage.prettify)


# In[71]:


menu=mainpage.ul.li.a


# In[73]:


print(menu)


# In[100]:


homepage=soup.find('div',class_="gridContainer clearfix")


# In[101]:


print(homepage.prettify)


# In[ ]:




