#include <stdio.h>
#include <string.h>
#include <time.h>

static FILE * f[100];
static char * fn[100];
static int ofirst = 1;

#define min(x,y) (((x)>(y))?(y):(x))
#define BUFSIZE 1024
static int   buf[BUFSIZE];
static char cbuf[BUFSIZE];

int xfopn_(ifl,hfl,imode,nf)
     int  * ifl;
     char * hfl;
     int  * imode;
     int   nf;
{
  int i;
  char * c; 
  char * mode;

  if      (*imode==1) { mode="w"; }
  else if (*imode==2) { mode="a"; }   
  else                { mode="r"; }   

  if(ofirst) { for(i=0;i<100;i++) { f[i]=NULL; fn[i]=NULL; } ofirst=0; }
  
  strncpy(cbuf,hfl,nf);  cbuf[nf]='\0';
  if((c=strchr(cbuf,' '))!=NULL) {*c='\0'; }
  if (f[*ifl]!=NULL) fclose(f[*ifl]);
  f[*ifl]=fopen(cbuf,mode);
  if (f[*ifl]!=NULL) {
    fn[*ifl]=(char *) malloc(strlen(cbuf)+1);
    strcpy(fn[*ifl],cbuf);
    /* printf("file opend unit=%d(%d) : %s\n",*ifl,f[*ifl],cbuf); */
    return 0;
  } else {
    printf("file open error : %s\n",cbuf);
    return 1;
  }

}

int xfadd_(ifl,hfl,nf)
     int  * ifl;
     char * hfl;
     int   nf;
{
  int i;
  char * c; 

  if(ofirst) { for(i=0;i<100;i++) { f[i]=NULL; fn[i]=NULL; } ofirst=0; }

  f[*ifl]=(FILE *) 1;
  strncpy(cbuf,hfl,nf);  cbuf[nf]='\0';
  if((c=strchr(cbuf,' '))!=NULL) {*c='\0'; }
  fn[*ifl]=(char *) malloc(strlen(cbuf)+1);
  strcpy(fn[*ifl],cbuf);
  return 0;

}



void xfcls_(ifl)
     int   * ifl;
{
  if (f[*ifl]!=NULL) { 
    fclose(f[*ifl]);
    f[*ifl]=NULL;
    free(fn[*ifl]);
    fn[*ifl]=NULL;
  }
}

int xfrew_(ifl)
     int   * ifl;
{
  if (f[*ifl]!=NULL) { 
    if (f[*ifl]==(FILE *)1) {
      return 1;
    } else {
      fseek(f[*ifl],0L,SEEK_SET);
      return 0;
    }
  } else {
    return -1;
  }
}

int xfinqu_(ifl)
     int * ifl;
{
  if (f[*ifl]!=NULL) { return 1; }
  else               { return 0; }
}

int xfinqn_(hfl,ifl,nf)
     char * hfl;
     int * ifl;
     int   nf;
{
  int  i,nn;
  char *j;

  nn=nf;
  if((j=strchr(hfl,' '))!=NULL) nn=j-hfl;
  for(i=0; i<100; i++) {
    if(fn[i]!=NULL && strncmp(hfl,fn[i],nn)==0 && fn[i][nn]=='\0') {
      *ifl=i;
      return 1;
    }
  }
  *ifl=-1;
  return 0;
}

int xfrdh_(ifl,hh,nn)
     int  * ifl;
     char * hh;
     int  * nn;
{
  int n[1];
  int ir;

  ir=fread(buf,4,1,f[*ifl]);
  if (ir>0) { 
    xfbyte(buf,n,1);
    ir=fread(hh,1,*nn,f[*ifl]);
    if (*n>*nn) { ir=fseek(f[*ifl],(long)(*n-*nn),SEEK_CUR); }
    ir=fread(buf,4,1,f[*ifl]);    
  }
  if (ir>0) return 0;
  else      return 1;
}


int xfrdr_(ifl,dd,nn)
     int * ifl;
     int * dd;
     int * nn;
{
  int n[1];
  int ir, i, m;

  ir=fread(buf,4,1,f[*ifl]);
  if (ir>0) {
    xfbyte(buf,n,1);
    for(i=0;i<*nn;i+=BUFSIZE) {
      m=min(*nn-i,BUFSIZE);
      ir=fread(buf,4,m,f[*ifl]);
      xfbyte(buf,&dd[i],m);
    }    
    if (*n>(*nn)*4) { ir=fseek(f[*ifl],(long)(*n-(*nn)*4),SEEK_CUR); }
    ir=fread(buf,4,1,f[*ifl]);
  }
  if (ir>0) return 0;
  else      return 1;
}

int xfskr_(ifl)
     int * ifl;
{
  int n[1];
  int ir, i, m;

  ir=fread(buf,4,1,f[*ifl]);
  if (ir>0) {
    xfbyte(buf,n,1);
    ir=fseek(f[*ifl],(long)(*n+4),SEEK_CUR);
  }
  if (ir>0) return 0;
  else      return 1;
}

int xfwth_(ifl,hh,nn)
     int  * ifl;
     char * hh;
     int  * nn;
{
  int n[1];

  xfbyte(nn,n,1);
  fwrite(n,4,1,f[*ifl]);
  fwrite(hh,1,*nn,f[*ifl]);
  fwrite(n,4,1,f[*ifl]);
  return 0;
}

int xfwtr_(ifl,dd,nn)
     int * ifl;
     int * dd;
     int * nn;
{
  int n[1];
  int i, m, nn4;

  nn4=(*nn)*4;
  xfbyte(&nn4,n,1);
  fwrite(n,4,1,f[*ifl]);
  for(i=0;i<*nn;i+=BUFSIZE) {
      m=min(*nn-i,BUFSIZE);
      xfbyte(&dd[i],buf,m);
      fwrite(buf,4,m,f[*ifl]);
  }
  fwrite(n,4,1,f[*ifl]);
  return 0;
}


xfbyte(gd,bd,nr)
     char * gd;
     char * bd;
     int    nr;
{
  int i,j;
  char c;
  for(i=0,j=0;i<nr;i++,j+=4) {
    bd[j]   = gd[j+3];
    bd[j+1] = gd[j+2];
    bd[j+2] = gd[j+1];
    bd[j+3] = gd[j];
  }    
}    


int getpid_()
{
  return getpid();
}

int kill_(ipid)
     int ipid;
{
  return kill(ipid,5);
}

double clocks_()
{     
  return (double) clock()/CLOCKS_PER_SEC;
}

void times_(itime)
     int * itime;
{     
  struct tm * tmn;
  time_t tt[1];
  *tt = time(NULL);
  tmn=localtime(tt);
  itime[0]=tmn->tm_year;
  itime[1]=tmn->tm_mon+1;
  itime[2]=tmn->tm_mday;
  itime[3]=tmn->tm_hour;
  itime[4]=tmn->tm_min;
  itime[5]=tmn->tm_sec;
}

