rm -rf src
mkdir -p src/main/scala/psksvp
mkdir -p src/main/resources/psksvp
cp -R ../CodeWithJVM/src/psksvp/AbstractStateMachine src/main/scala/psksvp/. 
cp -R ../CodeWithJVM/src/psksvp/GUI src/main/scala/psksvp/.
cp -R ../CodeWithJVM/src/psksvp/Terminal src/main/scala/psksvp/.
cp ../CodeWithJVM/src/psksvp/package.scala src/main/scala/psksvp/.
