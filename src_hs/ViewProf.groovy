import groovy.transform.Canonical

import javax.swing.*
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeModel
import java.awt.*
import java.util.List

@Canonical class CallInfo { String place; String module; int entries; double totalTime;
  List<CallInfo> callees = []
  String getId() { module + '.' + place }
  String toString() { return "$place ($module)  time $totalTime%, count $entries" }
}

class MergedCallInfo extends CallInfo {
  MergedCallInfo(List<CallInfo> components) {
    super(components[0].place, components[0].module, components.collect{ it.entries}.sum() as int, components.collect{ it.totalTime}.sum() as double)
  }
}

@SuppressWarnings(["GrReassignedInClosureLocalVar", "GroovyMissingReturnStatement"])
CallInfo readProfile() {
  boolean started = false
  List<CallInfo> nodeStack = [new CallInfo("root", "root", 0, 0)]
  new File("Main.prof").eachLine { line ->
    if (line.startsWith("MAIN")) started = true
    if (!started) return

    int prefixLength = 0
    while (line[prefixLength] == " ") prefixLength++
    if (prefixLength == 0) return

    def areas = line.split(" +")
    def newNode = new CallInfo(place:areas[1], module:areas[2], entries:(areas[4] as double) as int, totalTime:areas[7] as double)

    def parent = nodeStack[prefixLength - 1]
    parent.callees.add(newNode)
    if (nodeStack.size() <= prefixLength) nodeStack.add(null)
    nodeStack[prefixLength] = newNode
  }
  return nodeStack[0]
}

List<CallInfo> flatList(CallInfo root) { [root] + root.callees.collect { flatList it }.flatten() }

@SuppressWarnings("GrReassignedInClosureLocalVar")
Map<String, MergedCallInfo> mergeCalls(CallInfo root) {
  Map<String, List<CallInfo>> result = [:]
  flatList(root).each { info ->
    def merged = result[info.id]
    if (!merged) result.put(info.id, merged = [])
    merged << info
  }
  return result.collectEntries { [it.key, new MergedCallInfo(it.value)] }
}


DefaultMutableTreeNode buildTree(CallInfo info) {
  def node = new DefaultMutableTreeNode()
  node.userObject = info
  for (callee in info.callees.sort { -it.totalTime }) {
    node.add(buildTree(callee))
  }
  return node
}

println "Reading snapshot"
def rootCall = readProfile()

println "Building method list"
List<CallInfo> merged = mergeCalls(rootCall).values().sort { -it.totalTime }

println "Initializing UI"
def frame = new JFrame()
frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
frame.title = "Profiled view"
def tree = new JTree()
tree.model = new DefaultTreeModel(buildTree(rootCall))
tree.expandRow(0)

def jList = new JList(merged.toArray())

def tabs = new JTabbedPane()
tabs.addTab("Plain tree", new JScrollPane(tree))
tabs.addTab("Method list", new JScrollPane(jList))
frame.contentPane.add(tabs)

frame.size = new Dimension(800, 500)
frame.visible = true
