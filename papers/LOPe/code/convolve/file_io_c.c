#define C_MAIN 0

#include <stdio.h>
//#include <gdal_priv.h>
#include <gdal.h>

#ifdef __cplusplus
extern "C"
{
#endif

int image_size(const char * filename, int * nx, int * ny)
{
   int header[20];
   int nParams, nf, payload_size;

   FILE * fp = fopen(filename, "rb");

   fread(header, sizeof(int), 20, fp);
   
   nParams = header[1];
   *nx = header[3];
   *ny = header[4];
   nf = header[5];
   payload_size = header[7];

   printf("params: %d %d %d %d %d\n",  nParams, *nx, *ny, nf, payload_size);

   fclose(fp);

   return payload_size;
}

int read_image(const char * filename, char * image)
{
   int header[20];

   FILE * fp = fopen(filename, "rb");

   int numRead = fread(header, sizeof(int), 20, fp);

   numRead = fread(image, sizeof(char), header[7], fp);
   fclose(fp);

   printf(" %d %d %d %d %d %d\n", (int)image[0], (int)image[1], (int)image[2],
                                  (int)image[3], (int)image[4], (int)image[5]);
   printf(" %d %d %d %d %d %d\n", (unsigned char)image[0], (unsigned char)image[1], (unsigned char)image[2],
                                  (unsigned char)image[3], (unsigned char)image[4], (unsigned char)image[5]);

   return numRead;
}

void read_image_file(const char * filename, int nx, int ny, float * image)
{
   int i;

   GDALAllRegister();

   GDALDatasetH dataset = GDALOpen(filename, GA_ReadOnly);

   int xImageSize = GDALGetRasterXSize(dataset);
   int yImageSize = GDALGetRasterYSize(dataset);
   int nBands = GDALGetRasterCount(dataset);

   int xOffset = 0;
   int yOffset = 0;

   printf("read_image_file %s %d %d %d\n", filename, xImageSize, yImageSize, nBands);
   printf("   nx==%d ny==%d\n", nx, ny);

   float * buf = (float*) malloc(nx*ny*nBands*sizeof(float));
   //   unsigned char * newbuf = (float*) malloc(nx*ny*sizeof(float));

   //   GDALDatasetRasterIO(dataset, GF_Read, xOffset, yOffset, nx, ny, buf, nx, ny,
   //                       GDT_Byte, nBands, NULL, nBands, nBands*nx, 1);

   int tsize = 4;

   GDALDatasetRasterIO(dataset, GF_Read, xOffset, yOffset, nx, ny, buf, nx, ny,
                       GDT_Float32, nBands, NULL, nBands*tsize, nBands*nx*tsize, tsize);
   GDALClose(dataset);

   //   for (i=0; i<21; i++) {
   //      printf("buf[%d]=%f\n", i, buf[i]);
   //   }

   for (i = 0; i < nx*ny; i++) {
      float a = (buf[3*i+0] + buf[3*i+1] + buf[3*i+2])/3.0;
      image[i] = (unsigned char) a;
      buf[i] = a;
      if (i < 7) printf("buf[%d]=%d %f\n", i, (int)image[i], buf[i]);
   }
   free(buf);

   write_image_file("junk.tif", nx, ny, image);

#ifdef NOTME
   //printf("buf=%f %f %f %f %f %f\n", buf[0], buf[1], buf[2], buf[3], buf[4], buf[5]);

   GDALDriverH driver = GDALGetDriverByName("GTiff");
   if (driver == NULL) {
      exit(1);
   }

   nBands = 1;
   tsize = 1;

   dataset = GDALCreate(driver, "hellno.tif", nx, ny, nBands, GDT_Byte, NULL);
   if (dataset == NULL) {
      fprintf(stderr, "write_image_file: failed to open file %s\n", filename);
   }

   GDALDatasetRasterIO(dataset, GF_Write, xOffset, yOffset, nx, ny, newbuf, nx, ny,
                       GDT_Byte, nBands, NULL, nBands*tsize, nBands*nx*tsize, 1*tsize);

   GDALClose(dataset);
#endif
}

void write_image_file(const char * filename, int nx, int ny, float * image)
{
   int i;
   int nBands = 1;
   printf("writing image file %s (%d,%d)\n", filename, nx, ny);

   unsigned char * buf = (unsigned char*) malloc(nx*ny*sizeof(unsigned char));

   for (i = 0; i < nx*ny; i++) {
      buf[i] = (unsigned char) image[i];
   }

   GDALAllRegister();

   GDALDriverH driver = GDALGetDriverByName("GTiff");
   if (driver == NULL) {
      exit(1);
   }

   GDALDatasetH dataset = GDALCreate(driver, filename, nx, ny, nBands, GDT_Byte, NULL);
   if (dataset == NULL) {
      fprintf(stderr, "write_image_file: failed to open file %s\n", filename);
   }

   GDALDatasetRasterIO(dataset, GF_Write, 0, 0, nx, ny, buf, nx, ny, GDT_Byte,
                       nBands, NULL, 1, nx, nx*ny);

   GDALClose(dataset);
   free(buf);
}

int print_header(const char * filename)
{
   int header[20];
   int i, nParams, nx, ny, nf, payload_size;

   unsigned char payload[280878];

   FILE * fp = fopen(filename, "rb");

   int numRead = fread(header, sizeof(int), 20, fp);
   
   nParams = header[1];
   nx = header[3];
   ny = header[4];
   nf = header[5];
   payload_size = header[7];

   printf("params: %d %d %d %d %d\n",  nParams, nx, ny, nf, payload_size);

   //   payload_size = ;
   numRead = fread(payload, sizeof(char), payload_size, fp);
   printf("number Read: %d\n", numRead);

   for (i = 0; i < 20; i++) {
      printf("   [%d] = %d\n",  i, (int) payload[i]);
   }

   fclose(fp);

   return numRead;
}

#ifdef __cplusplus
}
#endif

#if C_MAIN == 1
int main(int argc, char* argv[])
{
   print_header("lena-sjooblom.pvp");
   return 0;
}
#endif
