#include <GLUT/glut.h> 
#include <OpenGL/gl.h> 
#include <OpenGL/glu.h> 
#include <OpenGL/OpenGL.h> 
#include <OpenGL/CGLRenderers.h>

#include <stdio.h>

// 1
CGLPixelFormatAttribute attribs[] = { 
 kCGLPFADisplayMask, 0, 
 //kCGLPFAFullScreen, 
 kCGLPFADoubleBuffer, 
 kCGLPFASampleBuffers, 1, 
 kCGLPFASamples, 2, 
 kCGLPFAMultisample,
 // kCGLPFANoRecovery, 
  kCGLPFARendererID, 
  kCGLRendererGenericFloatID, /* floating point software renderer */
 0 
}; 

CGLPixelFormatObj pixelFormat = 0; 
CGLContextObj context = 0; 
CGLContextObj orig_context = 0; 
long numPixelFormats = 0; // 2

#ifdef __APPLE__

CGLContextObj GetCurrentContext( GLvoid )
{ return CGLGetCurrentContext(); }


CGLContextObj CreateContext( CGLContextObj _ctx )
{
  CGLContextObj tmp;
  
  CGLPixelFormatObj pxf;
  
  GLint n_pxf;
  
  CGLPixelFormatAttribute attribs[] =
    { (CGLPixelFormatAttribute)NULL };
  
  CGLChoosePixelFormat( attribs, &pxf, &n_pxf );
  
  CGLCreateContext( pxf, _ctx, &tmp );
  
  return tmp;
}


GLboolean SetCurrentContext( CGLContextObj _ctx )
{ return CGLSetCurrentContext( _ctx ); }


GLvoid DestroyContext( CGLContextObj _ctx )
{ CGLDestroyContext( _ctx ); }
#endif

GLint screen = 0;

void hello() {
  printf("Hello\n");
  return;

  orig_context = CGLGetCurrentContext();
  //  CGGetVirtualScreen(orig_context,&screen);
  attribs[1] = CGDisplayIDToOpenGLDisplayMask (CGMainDisplayID ());  // 3
  CGLChoosePixelFormat (attribs, &pixelFormat, &numPixelFormats); 
  if (pixelFormat) {  // 4
     printf("Hello (pixelFormat)\n");
    CGLCreateContext (pixelFormat, orig_context, &context); 
    printf("%d",context);
    // CGLSetCurrentContext(context);
    CGLDestroyPixelFormat (pixelFormat); 
  }
}


