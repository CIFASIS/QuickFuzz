#include <gdk-pixbuf/gdk-pixbuf.h>
//#include <gtk/gtk.h>
// gcc gdk-pixbuf.c -o gdk-pixbuf  `pkg-config --libs --cflags gdk-pixbuf-2.0`

int main(int argc, char **argv) {
    //GtkImage *preview_widget = GTK_IMAGE(NULL);
    GdkPixbuf* buf;
    int size = 180;
    GError* err = NULL;
    gint preview_width = 0;
    gint preview_height = 0;

    GdkPixbufFormat *preview_format = gdk_pixbuf_get_file_info(argv[1],
                                                             &preview_width,
                                                             &preview_height);

    buf =  gdk_pixbuf_new_from_file(argv[1], &err);

    if (! err) {
      gdk_pixbuf_flip(buf, FALSE);
      gdk_pixbuf_rotate_simple(buf, 90);
      gdk_pixbuf_new_from_file_at_size(argv[1], size, size, &err);
    }


    //gtk_image_set_from_pixbuf(preview_widget, buf);

    if (err)
      printf ("Gerror: %s\n", err->message);

    g_object_unref(buf);
    return 0;
}
