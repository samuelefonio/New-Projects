class Node(object):
    
    def __init__(self,data,next=None):
        """
        Simple node with data and next
        """
        self.data=data
        self.next=next
    
class TwoWayNode(Node):
    
    def __init__(self,data,previous=None,next=None):
        """
        TwowayNode uses inheritance from Node to get data and next, and add previous as parameter
        """
        Node.__init__(self,data,next)
        self.previous=previous
    
    def get_data(self):
        return (self.data)
    
    def get_next(self):
        return (self.next)
    
    def get_previous(self):
        return (self.previous)
    
    def set_next_none(self):
        self.next=None
    
    def __getitem__(self,i):
        if i==0:
            return(self.data[0])
        elif i==1:
            return(self.data[1])
        else:
            raise IndexError
    
    def __str__(self):
        return str(self.data)
    
class Deque(object):
    
    def __init__(self,front=None,back=None):
        self.front=front
        self.back=back
        self.n=0
    
    def get_front(self):
        return (self.front.data)
    
    def is_empty(self):
        return self.front==None
    
    def enqueue_front(self,item):
        new_node=TwoWayNode(item)
        if self.is_empty()==True:
            self.front=new_node
            self.back=new_node
            self.n+=1
        else:
            s=self.front
            self.front.next=new_node
            self.front=self.front.next
            self.front.previous=s
            self.n+=1
    
    def enqueue_back(self,item):
        """
        Add from the back of the queue.
        Note: This operation does not increment the length of the queue since we use it just to rotate the queue.
        """
        new_node=TwoWayNode(item)
        if self.is_empty()==True:
            self.front=new_node
            self.back=new_node
        else:
            s=self.back
            self.back.previous=new_node
            self.back=self.back.previous
            self.back.next=s
    
    def pop(self):
        """
        This function just pop from the front of the queue, because we will not need the popleft.
        """
        s=self.front
        self.front=self.front.previous
        self.front.set_next_none()
        self.n-=1
        return(s)
    
    def rotate(self):
        """
        This function takes the last item and put it at the beginning of the queue
        """
        t=self.pop()
        self.enqueue_back(t)
    
    def __getitem__(self,index):
        if index>len(self):
            raise IndexError("Index out of bounds")
        i=0
        probe=self.back
        while i<index:
            probe=probe.get_next()
            i+=1
        return(probe)
    
    def __len__(self):
        return (self.n)
    
    def __str__(self):
        s = ""
        if self.back == None:
            return "None"
        current = self.back
        while current:
            s += str(current)
            current = current.get_next()
        return s


class Graph():

    def __init__(self):
        """
        This graph is represented using an adjacency set, not list as in setup_1.
        """
        self.graph = {}
    
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
        _edges = set()
        for n1 in self.graph:
            for n2 in self.graph[n1]:
                _edges.add((n1,n2))
        return _edges
    
    def single_edges(self):
        """
        Function that returns the edges of the graph. 
        input: - Graph
        output: - Edges of the graph
        Note: here there are not all the edges on which a path can move, since there are not the "inverted" edges.
        """
        _edges=set()
        for n1 in self.graph:
            for n2 in self.graph[n1]:
                if ((n1,n2) not in _edges) and ((n2,n1) not in _edges):
                    _edges.add((n1,n2))
        return _edges

    def insertVertex(self, v):
        """
        Function that add the vertex v.
        input: - v : vertex to add
        """
        if v not in self.graph:
            self.graph[v] = set()

    def insertEdge(self, v1,v2):
        """
        Function that add the (u,v) edge to the graph.
        input: - u,v : vertices of the edge to add.
        output: \
        """
        self.insertVertex(v1)
        self.insertVertex(v2)
        self.graph[v1].add(v2)
        self.graph[v2].add(v1)      

    def removeEdge(self, v1,v2):
        """
        Function that removes the (u,v) edge from the graph.
        input: - u,v : vertices of the edge to be cut.
        output: \
        """
        if v1 in self.graph:
            self.graph[v1].discard(v2)
        if v2 in self.graph:
            self.graph[v2].discard(v1)
    
    
    
    def DFSCount(self, v, visited={}):
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
    
    
    def is_connected(self):
        """
        Function that check if the graph is connected. It's enough checking for one vertex since it's an undirected graph.
        input: self
        output: True if the graph is connected
        """
        u=self.graph()[0]
        visited ={v:False for v in self.vertices()}
        count=self.DFSCount(u, visited)
        if count!=len(self.vertices()):
            return False
        return True
    
    def degree(self):
        """
        Function that returns the degree of each vertex.
        input: - Graph
        output: - Dictionary with the degree of every vertex.
        """
        degrees={vert:0 for vert in self.vertices()}   #We initialize every degree at 0
        for v in self.vertices():          #Cycle on the vertices
            degrees[v]=len(self.graph[v])
        return degrees
    
    def h_check(self):
        """
        Function that checks if the graph is fine. In this case all the vertices must have even degree.
        """
        for i in list(self.degree().values()):
            if i%2==1:
                return False
        return True

    
# HIERHOLZER
    
    def hierholzer_deq(self,start):
            """
            Function that uses a deque in order to get an Eulerian path (cycle) putting together many Eulerian cycles.
            The starting point is irrelevant, since during the running of the algorithm the deque rotates and it does not
            rerotate, because this would mean an important waste of time.
            The start is specified anyway, since for testing it we needed to change the starting point and see if it worked.
            Input: - Graph
                   - start : starting vertex
            output: Eulerian cycle
            """
            unvisited=self.edges()
            final=Deque()
            #final=c.deque()
            current=(start,list(self.graph[start])[0])
            
            
            while len(unvisited)!=0:
                
                final.enqueue_front(current)
                #final.append(current)
                
                
                unvisited.discard(current)
                unvisited.discard((current[1],current[0]))
                
                self.removeEdge(current[0],current[1])
                
                new=current[1]
                
                if len(unvisited)==0: 
                    #print(final) 
                    return (final)
                
                if (len(self.graph[new])==0) and len(unvisited)>0: #We got stuck and it's not over yet
                    #print("if")
                    #t=final[-1]
                    
                    t=final.get_front()
                    while len(self.graph[t[1]])==0:
                        #print("rotate")
                        #temp=final.pop()
                        #final.appendleft(temp)
                        final.rotate()
                        #t=final[-1]
                        
                        t=final.get_front()
                    new=t[1]                        
                
                current=(new,list(self.graph[new])[0])
            return
            
        
    def hierholzer2_deq(self,start):
        """
        This function just put together the check and the algorithm.
        """
        if self.h_check()==False:
            #print("none")
            return None
        else:
            return(self.hierholzer_deq(start))