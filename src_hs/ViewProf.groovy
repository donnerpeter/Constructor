import javax.swing.*
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeModel
import java.awt.Dimension

def frame = new JFrame()
frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
frame.title = "Profiled view"
def tree = new JTree()
frame.contentPane.add(new JScrollPane(tree))
def root = new DefaultMutableTreeNode("root")
def model = new DefaultTreeModel(root)
tree.model = model

boolean started = false
List<DefaultMutableTreeNode> nodeStack = [root]
new File("Main.prof").eachLine { line ->
  if (line.startsWith("MAIN")) started = true
  if (!started) return

  int prefixLength = 0
  while (line[prefixLength] == " ") prefixLength++
  if (prefixLength == 0) return

  def areas = line.split(" +")
  def cc = areas[1]
  def module = areas[2]
  def entries = areas[4]
  def totalTime = areas[7]

  def newNode = new DefaultMutableTreeNode("$cc   time $totalTime, count $entries")

  def parent = nodeStack[prefixLength - 1]
  parent.add(newNode)
  model.nodeStructureChanged(parent)
  if (nodeStack.size() <= prefixLength) nodeStack.add(null)
  nodeStack[prefixLength] = newNode
}

tree.expandRow(0)

frame.size = new Dimension(500, 500)
frame.visible = true
