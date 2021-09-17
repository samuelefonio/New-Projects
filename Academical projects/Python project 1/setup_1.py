

from collections import defaultdict

#This class represents an undirected graph using adjacency list representation
class Graph():

    def __init__(self):
        
        """
        The graph is represented through an adjacency list.
        input: - vertices : Number of vertices
        output: - Graph 
        """
        
        self.graph = defaultdict(list) # default dictionary to store graph
        
    def vertices(self):
        
        """
        Function that returns the vertices of the graph. 
        input: - Graph
        output: - vertices of the graph
        """
        
        return list(self.graph.keys())
    
    def edges(self):
            
        """
        Function that returns the whole set of edges, in both order. It means that there is not just (a,b) bust also (b,a). 
        input: - Graph
        output: - list of edges
        """
        
        edges=[]
        for i in self.vertices():
            for j in self.graph[i]:
                edges.append((i,j))
        return(edges)
    
    def single_edges(self):
        
        """
        Function that returns the edges of the graph. 
        input: - Graph
        output: - Edges of the graph
        Note: here there are not all the edges on which a path can move, since there are not the "inverted" edges.
        """
        
        edges=set()
        for i in self.graph:
            for j in self.graph[i]:
                current=(i,j)
                invert_current=(j,i)
                if invert_current in edges:
                    continue
                else:
                    edges.add(current)
        return(edges)
    
    def addVertex(self, v):
        
        """
        Function that add the vertex v.
        input: - v : vertex to add
        """
        
        self.graph[v]=[]
        
    def addEdge(self,u,v):
        
        """
        Function that add the (u,v) edge to the graph.
        input: - u,v : vertices of the edge to add.
        output: \
        """
        if v=='d':
            self.graph[u].insert(0,v)
            self.graph[v].append(u)
        else:
                    
            self.graph[u].append(v)
            self.graph[v].append(u)

    def removeEdge(self, u, v):
        
        """
        Function that removes the (u,v) edge from the graph.
        input: - u,v : vertices of the edge to be cut.
        output: \
        """
        self.graph[u].remove(v)
        self.graph[v].remove(u)

    def DFSCount(self, v, visited=defaultdict(list)):
        
        """
        Function based on the Depth First Search which counts the reachable vertices from v.
        input: - v : starting vertex
               - visited : disctionary to store the passed vertices
        output: Number of vertices reachable
        """
        
        count = 1
        visited[v] = True
        for i in self.graph[v]:
            if visited[i] == False:
                count = count + self.DFSCount(i, visited)
        return count    
    
    
    def degree(self):
        
        """
        Function that returns the degree of each vertex.
        input: - Graph
        output: - Dictionary with the degree of every vertex.
        """
        
        degrees={vert:0 for vert in self.vertices()}   #We initialize every degree at 0
        for v in self.vertices():                      #Cycle on the vertices
            degrees[v]=len(self.graph[v])
        return degrees
    
    def is_connected(self):
        
        """
        Function that check if the graph is connected. It's enough checking for one vertex since it's an undirected graph.
        input: self
        output: True if the graph is connected
        """
        
        u=self.vertices()[0]
        visited ={v:False for v in self.vertices()}
        count=self.DFSCount(u, visited)
        if count!=len(self.vertices()):
            return False
        return True
    
    
        
        
# Hiezholzer Algorithm
        
    def ep_check(self):
        
        """
        This function checks if the graph satisfies the requirements for having an Eulerian path:
        We have to check that, if there are odd degrees they must 2.
        """
        
        if self.is_connected()==False:
            return False
        odd=0
        even=0
        for i in list(self.degree().values()):
            if i%2==0:
                even+=1
            else:
                odd+=1
        deg=[odd,even]
        
        if even==len(self.vertices()):                    #All vertices have even degree
            return True
        elif (even==len(self.vertices())-2) and (odd==2): #There are only 2 vertices with odd degree
            return True
        else:
            return False
        
    def hierholzer_rec(self, start, unvisited,index=0,final=[]): 
        
        """
        Function (recursive) to return the Eulerian path. Note, the check for the existence of an Eulerian path will be done using another function.
        The graph traversal is based directly on the graph.
        input: -start: starting vertex
               -unvisited: list of unvisited edges (to start they are all unvisited, it will be initialized by all_edges)
               -visited: refers to the visited vertices
               -final: refers to the visited edges
        output: Eulerian path that traverse every edge just once.
        """
        
        if len(self.graph[start])!=0 and len(unvisited)>0:   #If from the vertex in which we are we can move in any way and there are edges unvisited
            
            current=(start,self.graph[start][0])                  #we choose an edge
            
            #visited.insert(index,start)                      #We append the start to our path
            final.insert(index,current)                      #We append the edge to our path
            #print(final)
            index+=1
            
            current_inverted=(current[1],current[0])
            
            self.removeEdge(start,current[1])
            unvisited.remove(current)                  #We cut off the current edge from the unvisited
            unvisited.remove(current_inverted)
            
            start=current[1]                           #We move on and start the iteration
            #print(self.graph[start])
            #while len(unvisited)>0:      #Or we have visited all the edges or we got a circle
                #print(start)
            self.hierholzer_rec(start,unvisited,index,final)
                
        elif len(self.graph[start])==0 and len(unvisited)>0:           #We got a cycle
            i=len(final)-1
            while len(self.graph[final[i][1]])==0:
                #print("while")
                if i==0:
                    break
                i-=1
            if i==0 and len(self.graph[final[i][1]])==0:  #If arrived in the fisrt edge, from the second vertex I can't move.
                #print("if")
                index=0
                start=final[0][0]
                self.hierholzer_rec(start,unvisited,index,final)
            else:
                #print("else")
                index=i+1
                start=final[i][1]
                self.hierholzer_rec(start,unvisited,index,final)
            
        return(final)
        
    
    def hierholzer2_rec(self,start,unvisited,index=0,final=[]):
        
        """
        Function  to put together the check and the hierholzer function. It choses as start a node with odd degree.
        input: -start: starting vertex
               -unvisited: list of unvisited edges (to start they are all unvisited, it will be initialized by all_edges)
               -possibilities: initially set equal to self.v_edges
               -visited: refers to the visited vertices
               -final: refers to the visited edges
        output: Eulerian path that traverse every edge just once.
        """
        
        if self.ep_check()==False:
            return None
        if self.degree()[start]%2 != 0:
            return(self.hierholzer_rec(start,unvisited,index=0,final=[]))
        else:
            for i in self.degree():
                if self.degree()[i]%2 != 0 :
                    start = i
                    break
                else:
                    pass
            return(self.hierholzer_rec(start,unvisited,index=0,final=[]))
    
    def hierholzer_it(self,start):
            
        """
        Function (iterative) to return the Eulerian path. Note, the check for the existence of an Eulerian path will be done using another function.
        The graph traversal is based directly on the graph.
        input: -start: starting vertex
               -unvisited: list of unvisited edges (to start they are all unvisited, it will be initialized by all_edges)
               -visited: refers to the visited vertices
               -final: refers to the visited edges
        output: Eulerian path that traverse every edge just once.
        """
            
        unvisited=self.edges()
        final=[]
        current=(start,self.graph[start][0])
            
        index=0
            
            
        while len(unvisited)!=0:
                
            
            final.insert(index,current)
            index+=1
                
            self.removeEdge(current[0],current[1])
            unvisited.remove(current)
            unvisited.remove((current[1],current[0]))
                
            start=current[1]
                
                
            if len(unvisited)==0:
                return(final)
                
            if (len(self.graph[start])==0) and len(unvisited)==0:
                final.insert(index,current)
                return(final)
                
            if (len(self.graph[start])==0) and len(unvisited)>0: 
                i=len(final)-1
                    
                while len(self.graph[final[i][1]])==0:
                 
                    if i==0:
                        break
                    i-=1
                if i==0 and len(self.graph[final[i][1]])==0:
                  
                    index=0
                    start=final[0][0]
                else:
                    index=i+1
                    start=final[i][1]
            current=(start,self.graph[start][0])     
        return (final)
        
    def hierholzer2_it(self,start):
        
        """
        Function (iterative) to put together the check and the hierholzer function. It choses as start a node with odd degree.
        input: -start: starting vertex
               -unvisited: list of unvisited edges (to start they are all unvisited, it will be initialized by all_edges)
               -possibilities: initially set equal to self.v_edges
               -visited: refers to the visited vertices
               -final: refers to the visited edges
        output: Eulerian path that traverse every edge just once.
        """
        if self.ep_check()==False:
            return None
        if self.degree()[start]%2 != 0:
            return(self.hierholzer_it(start))
        else:
            for i in self.degree():
                if self.degree()[i]%2 != 0:
                    start = i
                    break
                else:
                    continue
            #print(start)
            return(self.hierholzer_it(start))

        
        
        
        
#Fleury's Algorithm

    def isValidNextEdge(self, u, v):
        
        """
        Function that returns if an edge is valid for the path built by Fleury's algorithm, i.e. it does not disconnect the graph.
        input: - u,v: interested edge
        output: True if it's valid, i.e. it does not disconnect the graph
        """
        
        if len(self.graph[u]) == 1:
            return True
        if len(self.graph[u]) ==0:
            return False
        else:
            visited ={v:False for v in self.vertices()}
            count1 = self.DFSCount(u, visited)

            self.removeEdge(u, v)
            
            visited ={v:False for v in self.vertices()}
            count2 = self.DFSCount(u, visited)

            self.addEdge(u,v)

            return False if count1 > count2 else True

    def fleuryUtil_rec(self,u,final=[]):
        
        """
        Function (recursive) to return the Eulerian path folllowing the Fleury's algorithm. The check will be done using another function.
        input: - u : starting vertex
               - final=[] : final path
        output: final
        """
        
        for v in self.graph[u]:
            if self.isValidNextEdge(u,v):
                final.append((u,v))
                self.removeEdge(u,v)
                self.fleuryUtil_rec(v,final)
        return(final)
    
    def fleury_rec(self,u):
        
        """
        Function to put together the check and the fleuryUtil function.
        input: - u : starting vertex
        output: final
        """

        if self.ep_check()==False:
            return None
        if self.degree()[u]%2 != 0:
            return(self.fleuryUtil_rec(u,final=[]))
        else:
            for i in self.degree():
                if self.degree()[i]%2 != 0 :
                    u = i
                    break
            return(self.fleuryUtil_rec(u,final=[]))
        
    
        
    def fleuryUtil_it(self,u,l):
        """
        This iterative function returns a Eulerian Path when possible, starting from vertex u.
        u: starting vertex, chosen by self.fleury(l)
        l: Must be initialized by len(self.single_edges())
        output: list of edges.
        """
        final=[]
        v=list(self.graph[u])[0]
        while len(final)!=l:
            if len(self.graph[u])==0:
                print("here")
                return(final)
            i=0
            v=list(self.graph[u])[i]
            while self.isValidNextEdge(u,v)==False:
                i+=1
                v=list(self.graph[u])[i]
            final.append((u,v))
            self.removeEdge(u,v)
            u=v   
        return(final)
                    
                    
    def fleury_it(self,u,l):
        
        """
        Function to put together the check and the fleuryUtil function.
        input: - u : starting vertex
        output: final
        """
        if self.ep_check()==False:
            return None
        if self.degree()[u]%2 != 0:
            return(self.fleuryUtil_it(u,l))
        else:
            for i in self.degree():
                if self.degree()[i]%2 != 0 :
                    u = i
                    break
            # Print tour starting from odd vertex
            return(self.fleuryUtil_it(u,l))