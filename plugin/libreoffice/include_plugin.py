import zipfile
import shutil
import os
import sys


def zipdir(path, ziph):
    # ziph is zipfile handle
    for root, dirs, files in os.walk(path):
        for file in files:
            dest = os.path.relpath(os.path.join(root, file), os.path.join(path, '..'))
            print ("Dest " + dest)
            ziph.write(os.path.join(root, file), 
              dest)

print("Delete and create directory with_macro")
shutil.rmtree("with_macro",True)
os.mkdir("with_macro")

filename = "with_macro/"+sys.argv[1]
scriptName = sys.argv[2]
print("Open file " + sys.argv[1]  +  " Using script " + scriptName)
shutil.copyfile(sys.argv[1],filename)


doc = zipfile.ZipFile(filename,'a')
## XXX: Use python based path separators
doc.write(scriptName, "Scripts/python/" + scriptName)

# Copy the certificate bundle.
manifest = []
for line in doc.open('META-INF/manifest.xml'):
  if '</manifest:manifest>' in line.decode('utf-8'):
    for path in ['Scripts/','Scripts/python/','Scripts/python/' + scriptName, 'Scripts/beta_ccardemo_tech.ca-bundle']:
      manifest.append(' <manifest:file-entry manifest:media-type="application/binary" manifest:full-path="%s"/>' % path)
  manifest.append(line.decode('utf-8'))


# for line in doc.open('META-INF/manifest.xml'):
#   if '</manifest:manifest>' in line.decode('utf-8'):
#     for path in ['certificates']:
#       manifest.append(' <manifest:file-entry manifest:media-type="text" manifest:full-path="%s"/>' % path)
#   manifest.append(line.decode('utf-8'))


doc.writestr('META-INF/manifest.xml', ''.join(manifest))
doc.close()
print("File created: "+filename)