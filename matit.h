#ifndef MATRIX_H
#define MATRIX_H

#include <cstdlib>
#include <cstdio>
#include <cassert>
#include <cstdarg>
#include <cstring>
#include <string>
using namespace std;



#define MPRINT(a)				\
  printf("%s\n",#a);				\
  mprint(a)

#define mat_for(mat)				\
  for(int c=0;c<mat.ncol();c++)			\
    for(int r=0;r<mat.nrow();r++)

template<class T>
struct Matrix{//Column Major
  T *dat;
  
  int nr;
  int nc;
  /*
    <---------> nc
    A0 4 ...   N
    |1 5 (r,c)
    |2 6 
    V3 7
    nr == 4

  */
  inline
  T  get(int r,int c) const{
    assert(0<=r);
    assert(r<nr);
    assert(0<=c);
    assert(c<nc);
    return dat[r+c*nr];
  }
  inline
  void set(int r,int c,T v){
    assert(0<=r);
    assert(r<nr);
    assert(0<=c);
    assert(c<nc);
    dat[r+c*nr]=v;
  }
  void
  set(T* ary){
    for(int i=0;i<nc*nr;i++)
      this->set(i/nc,i%nc,ary[i]);
  }
  void
  set_row(int row,T* ary){
    for(int i=0;i<nc;i++)
      this->set(row,i,ary[i]);
  }
  void
  set_col(int col,T* ary){
    for(int i=0;i<nr;i++)
      this->set(i,col,ary[i]);
  }
  void
  zero(){
    memset(dat,0,nr*nc*sizeof(T));
    /* for(int i=0;i<nc*nr;i++) */
    /*   this->set(i/nc,i%nc,0); */
  }

  T& operator()(int row,int col){
    assert(0<=row);
    assert(row<nr);
    assert(0<=col);
    assert(col<nc);
    return dat[col*nr+row];
  }

  int pos;
  Matrix& operator<<(T v){
    this->set(0,0,v);
    pos=1;
    return *this;
  }

  Matrix& operator,(T v){
    this->set(pos/nc,pos%nc,v);
    pos++;
    return *this;
  }

  Matrix& operator+=(const Matrix& b){
    for(int k=0;k<this->nc;k++)
      for(int i=0;i<this->nr;i++)
	(*this)(i,k)+=b.get(i,k);
    return *this;
  }

  Matrix& operator-=(const Matrix& b){
    for(int k=0;k<this->nc;k++)
      for(int i=0;i<this->nr;i++)
	(*this)(i,k)-=b.get(i,k);
    return *this;
  }

  Matrix(){
    dat=NULL;
    nr=0;
    nc=0;
  }

  Matrix(int nr,int nc){
    this->nr=nr;
    this->nc=nc;
    dat=(T*)malloc(nr*nc*sizeof(T));
  }

  Matrix(const Matrix& m){
    nr=m.nr;
    nc=m.nc;
    dat=(T*)malloc(nr*nc*sizeof(T));
    memcpy(dat,m.dat,sizeof(T)*nc*nr);
  }
  Matrix&
  operator=(const Matrix& m){
    if(dat!=NULL){
      free(dat);
      dat=NULL;
    }
    
    nr=m.nr;
    nc=m.nc;
    dat=(T*)malloc(nr*nc*sizeof(T));

    memcpy(dat,m.dat,sizeof(T)*nc*nr);
    return *this;
  }
  ~Matrix(){
    if(dat!=NULL)
      free(dat);
    dat=NULL;
  }
  inline
  int nrow() const{
    return nr;
  }
  inline
  int ncol() const{
    return nc;
  }
  void swap(Matrix& mat){
    {
      T* d=this->dat;
      this->dat=mat.dat;
      mat.dat=d;
    }
    
    int  a=this->nr;
    this->nr=mat.nr;
    mat.nr=a;
    
    a=this->nc;
    this->nc=mat.nc;
    mat.nc=a;
  }
  void
  mwritei(FILE* out)const{
    for(int r=0;r<nrow();r++){
      for(int c=0;c<ncol();c++)
	fprintf(out,
		"%3d%c",
		(int)get(r,c),
		(c==ncol()-1 ? ' ' : ',')
		);
      fprintf(out,"\n");
    }
  }

  void
  mreadi(FILE* in){
    char buf[2048];
    for(int r=0;r<nr;r++){
      if(fgets(buf,sizeof(buf),in)==NULL)
	break;
      char *p=buf;
      char *n;
      int d;
      int c=0;
      while(1){
	//skip space or '"'
	while(*p==' '||*p=='"')
	  p++;
	n=p;
	//seach ',' or 0
	while(*p!=','&&*p!=0)
	  p++;
    
	sscanf(n,"%d",&d);
	set(r,c,d);

	if(*p==0)
	  break;
	else if(*p==',')
	  p++;
	c++;
      }
    }
  }
  void
  mprint(){
    mwritei(stdout);
  }
  
};



template<class T>
void
mprint(const Matrix<T>& a){
  for(int r=0;r<a.nrow();r++){
    for(int c=0;c<a.ncol();c++)
      printf("%4d%c",
	     a.get(r,c),
	     (c==a.ncol()-1 ? ' ' : ',')
	     );
    printf("\n");
  }
  printf("\n");
}
 

template<class T>
Matrix<T>
operator +(const Matrix<T>& a,const Matrix<T>& b){
  Matrix<T> c=a;
  for(int j=0;j<c.nc;j++)
    for(int i=0;i<c.nr;i++)
      c(i,j)+=b.get(i,j);
  return c;
}

template<class T>
Matrix<T>
operator +(const Matrix<T>& a,T b){
  Matrix<T> c=a;
  for(int j=0;j<c.nc;j++)
    for(int i=0;i<c.nr;i++)
      c(i,j)+=b;
  return c;
}

template<class T>
Matrix<T>
operator -(const Matrix<T>& a,T b){
  Matrix<T> c=a;
  for(int j=0;j<c.nc;j++)
    for(int i=0;i<c.nr;i++)
      c(i,j)-=b;
  return c;
}

template<class T>
Matrix<T>
operator -(const Matrix<T>& a){
  Matrix<T> c=a;
  for(int j=0;j<c.nc;j++)
    for(int i=0;i<c.nr;i++)
      c(i,j)=-c(i,j);
  return c;
}

template<class T>
Matrix<T>
operator *(const Matrix<T>& a,T b){
  Matrix<T> c=a;
  for(int j=0;j<c.nc;j++)
    for(int i=0;i<c.nr;i++)
      c(i,j)*=b;
  return c;
}

template<class T>
Matrix<T>
operator /(const Matrix<T>& a,T b){
  Matrix<T> c=a;
  for(int j=0;j<c.nc;j++)
    for(int i=0;i<c.nr;i++)
      c(i,j)/=b;
  return c;
}

template<class T>
Matrix<T>
operator -(const Matrix<T>& a,const Matrix<T>& b){
  Matrix<T> c=a;
  for(int j=0;j<c.nc;j++)
    for(int i=0;i<c.nr;i++)
      c(i,j)-=b.get(i,j);
  return c;
}


template<class T>
Matrix<T>
operator *(const Matrix<T>& a,const Matrix<T>& b){
  Matrix<T> c(a.nrow(),b.ncol());
  c.zero();
  for(int j=0;j<c.nc;j++)
    for(int k=0;k<a.nc;k++)
      for(int i=0;i<c.nr;i++)
	c(i,j)+=a.get(i,k)*b.get(k,j);
  return c;
}

template<class T>
Matrix<T>
ary_mul(const Matrix<T>& a,const Matrix<T>& b){
  Matrix<T> c=a;
  for(int j=0;j<c.nc;j++)
    for(int i=0;i<c.nr;i++)
      c(i,j)*=b.get(i,j);
  return c;
}

template<class T>
Matrix<T>
ary_div(const Matrix<T>& a,const Matrix<T>& b){
  Matrix<T> c=a;
  for(int j=0;j<c.nc;j++)
    for(int i=0;i<c.nr;i++)
      c(i,j)/=b.get(i,j);
  return c;
}


template<class T>
bool
operator ==(Matrix<T>& a,Matrix<T>& b){
  if(a.nrow()==b.nrow()&&
     a.ncol()==b.ncol()){
    for(int c=0;c<a.ncol();c++)
      for(int r=0;r<a.nrow();r++)
	if(a.get(r,c)!=b.get(r,c))
	  return false;
    return true;
  }else{
    return false;
  }
}

template<class T>
bool
operator !=(Matrix<T>& a,Matrix<T>& b){
  return !(a==b);
}

template<class T>
Matrix<T>
operator ^(Matrix<T>& a,int v){
  Matrix<T> b=a;
  Matrix<T> c(a.nrow(),a.ncol());
  for(int i=1;i<v;i++){
    c=a*b;
    b.swap(c);
  }
  return b;
}


template<class T>
Matrix<T>
col_vec(const Matrix<T>& a,int col){
  Matrix<T> cc(a.nr,1);
  memcpy(cc.dat,a.dat+a.nr*col,sizeof(T)*a.nr);
  return cc;
}

template<class T>
Matrix<T>
row_vec(const Matrix<T>& a,int row){
  Matrix<T> cc(1,a.nc);
  for(int c=0;c<a.nc;c++)
    cc(0,c)=a.get(row,c);
  return cc;
}


template<class T>
int
maxrow(const Matrix<T>& a){
  assert(a.nc==1);
  T va=a.get(0,0);
  int idx=0;
  for(int x=1;x<a.nr;x++)
    if(va<a.get(x,0)){
      idx=x;
      va=a.get(x,0);
    }
  
  return idx;
}

template<class T>
int
minrow(const Matrix<T>& a){
  assert(a.nc==1);
  T va=a.get(0,0);
  int idx=0;
  for(int x=1;x<a.nr;x++)
    if(va>a.get(x,0)){
      idx=x;
      va=a.get(x,0);
    }
  
  return idx;
}

template<class T>
int
minrow(const T* a,int len){
  T va=a[0];
  int idx=0;
  for(int x=1;x<len;x++)
    if(va>a[x]){
      idx=x;
      va=a[x];
    }
  
  return idx;
}



template<class T>
Matrix<T>
t(const Matrix<T>& a){// return At
  Matrix<T> cc(a.ncol(),a.nrow());
  mat_for(cc)
    cc(r,c)=a.get(c,r);
  return cc;
}

template<class T>
T
dot(const Matrix<T>& a,const Matrix<T>& b){
  int s=0;
  for(int c=0;c<a.ncol();c++)
    for(int r=0;r<a.nrow();r++)
      s+=a.get(r,c)*b.get(r,c);
  return s;
}

template<class T>
T
nrm2(const Matrix<T>& a){
  int s=1;
  return dot(a,a);
}


template<class T>
T
ave(Matrix<T>& a){
  T t=0.0;
  mat_for(a)
    t+=a(r,c);
  return t/(a.nr*a.nc);
}

template<class T>
Matrix<T>
ave_row(const Matrix<T>& a){
  Matrix<T> tmp(1,a.nr);
  for(int x=0;x<a.nr;x++)
    tmp(0,x)=1;

  T alpha=1.0/a.nr;
  T beta=0;
  return tmp*a/a.nr;
}


template<class T>
T
var(Matrix<T>& a){
  T average=ave(a);
  T t=0.0;
  mat_for(a)
    t+=(a(r,c)-average)*(a(r,c)-average);
  return t/(a.nr*a.nc);
}

template<class T>
Matrix<T>
var_row(Matrix<T>& a){
  Matrix<T> av=ave_row(a);
  Matrix<T> r(1,a.nc);
  for(int y=0;y<a.nc;y++){
    r(0,y)=0;
    for(int x=0;x<a.nr;x++)
      r(0,y)+=(a(x,y)-av(1,y))*(a(x,y)-av(0,y));
    r(0,y)=r(0,y)/(a.nr);
  }
  return r;
}



template<class T>
Matrix<T>
solve_ls(const Matrix<T>& a,const Matrix<T>& B,const Matrix<T>& init,int num){
  Matrix<T> A=t(a)*a;
  Matrix<T> Y=t(a)*B;
  Matrix<T> LU(A.nr,A.nc);
  Matrix<T> D(A.nr,B.nc);
  Matrix<T> X(A.nc,B.nc);
  mat_for(X)
    X(r,c)=init.get(r,c);

  mat_for(A)
    if(r==c){
      for(int i=0;i<D.nc;i++)
	D(r,i)=A(r,c);
      //	D(r,i)=A(r,c)+1;
      LU(r,c)=0;
    }else{
      LU(r,c)=A(r,c);
    }
  /* MPRINT(t(a)); */
  /* MPRINT(B); */
  /* MPRINT(Y); */
  /* MPRINT(init); */
  for(int i=0;i<num;i++){
    X=ary_div((Y-LU*X),D);
    //    MPRINT(X);
  }

  return X;
}

template<class T>
T
max(const Matrix<T>& a){
  T v=a.get(0,0);
  mat_for(a)
    if(v<a.get(r,c))
      v=a.get(r,c);
  return v;
}

template<class T>
Matrix<T>
eigvec_power(const Matrix<T>& a,const Matrix<T>& init,int thresh,int num){
  Matrix<T> b=init;
  for(int i=0;i<num;i++){
    Matrix<T> c=a*b;
    int v=max(c);
    if(v>thresh)
      b=c/2;
    else
      b=c;
  }
  return b;
}

template<class T>
Matrix<T>
pca_power(const Matrix<T>& a,const Matrix<T>& init,int thresh,int num){
  Matrix<T> av=ave_row(a);
  Matrix<T> b=a;
  for(int y=0;y<a.nc;y++){
    for(int x=0;x<a.nr;x++)
      b(x,y)=b(x,y)-av(0,y);
  }
  
  Matrix<T> rr=t(b)*b;
  mat_for(rr)
    rr(r,c)=rr(r,c)/a.nr;
  return eigvec_power(rr,init,thresh,num);
}




template<class T>
void
mwritei(FILE* out,const Matrix<T>& a){
  a.mwritei(out);
}

template<class T>
void
mwritei(const char* out,const Matrix<T>& a){
  FILE* o=fopen(out,"wb");
  mwritei(o,a);
  fclose(o);
}


template<class T>
void
mreadi(FILE* in,Matrix<T>& a){
  a.mreadi(in);
}

template<class T>
void
mreadi(const char* in,Matrix<T>& a){
  FILE* i=fopen(in,"rb");
  mreadi(i,a);
  fclose(i);
}



//#undef T

#endif
