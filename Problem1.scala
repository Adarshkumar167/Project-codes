import scala.collection.mutable.ListBuffer

// Define case classes for Book and Student
case class Book(id: Int, title: String, author: String, var quantity: Int)
case class Student(id: Int, name: String)

object Library {
  // Initialize list buffers to store books and students
  val books = ListBuffer[Book]()
  val students = ListBuffer[Student]()

  // Function to show details of all customers or a specific customer
  def showCustomers(customerId: Option[Int]): Unit = {
    customerId match {
      case Some(id) => {
        val customer = students.find(_.id == id)
        customer match {
          case Some(c) => println(s"Customer ${c.id}: ${c.name}")
          case None => println("Customer not found")
        }
      }
      case None => students.foreach(c => println(s"Customer ${c.id}: ${c.name}"))
    }
  }

  // Function to add or remove a customer account
  def manageAccount(customerId: Int, add: Boolean): Unit = {
    val customer = students.find(_.id == customerId)
    customer match {
      case Some(c) => {
        if (add) {
          println(s"Account already exists for customer ${c.id}: ${c.name}")
        } else {
          students -= c
          println(s"Account removed for customer ${c.id}: ${c.name}")
        }
      }
      case None => {
        if (add) {
          students += Student(customerId, "")
          println(s"Account added for customer $customerId")
        } else {
          println("Customer not found")
        }
      }
    }
  }

  // Function to deposit money into a customer's account
  def depositMoney(customerId: Int, amount: Double): Unit = {
    val customer = students.find(_.id == customerId)
    customer match {
      case Some(c) => {
        println(s"Deposited $amount into account of customer ${c.id}: ${c.name}")
      }
      case None => println("Customer not found")
    }
  }

  // Function to add a new book
  def addBook(title: String, author: String, quantity: Int): Unit = {
    val newId = books.lastOption.map(_.id + 1).getOrElse(1)
    books += Book(newId, title, author, quantity)
    println(s"Book $newId added: $title by $author, Quantity: $quantity")
  }

  // Function to upgrade the quantity of a book
  def upgradeBook(bookId: Int, quantity: Int): Unit = {
    val book = books.find(_.id == bookId)
    book match {
      case Some(b) => {
        b.quantity += quantity
        println(s"$quantity copies of ${b.title} added")
      }
      case None => println("Book not found")
    }
  }

  // Function to search for a book by title or author
  def searchBook(query: String): Unit = {
    val matchingBooks = books.filter(b => b.title.contains(query) || b.author.contains(query))
    if (matchingBooks.isEmpty) {
      println("No books found")
    } else {
      matchingBooks.foreach(b => println(s"Book ${b.id}: ${b.title} by ${b.author}, Quantity: ${b.quantity}"))
    }
  }

  // Function to show details of all books
  def showBooks(): Unit = {
    books.foreach(b => println(s"Book ${b.id}: ${b.title} by ${b.author}, Quantity: ${b.quantity}"))
  }

  // Function to register a student
    def registerStudent(name: String): Unit = {
    val newId = students.lastOption.map(_.id + 1).getOrElse(1)
    students += Student(newId, name)
    println(s"Student $newId registered: $name")
  }

  // Function to show details of all registered students
  def showStudents(): Unit = {
    students.foreach(s => println(s"Student ${s.id}: ${s.name}"))
  }

  // Function to check out a book
  def checkOutBook(bookId: Int, customerId: Int): Unit = {
    val book = books.find(_.id == bookId)
    book match {
      case Some(b) => {
        if (b.quantity > 0) {
          val customer = students.find(_.id == customerId)
          customer match {
            case Some(c) => {
              b.quantity -= 1
              println(s"${b.title} checked out by ${c.name}")
            }
            case None => println("Customer not found")
          }
        } else {
          println(s"No copies of ${b.title} available for checkout")
        }
      }
      case None => println("Book not found")
    }
  }

  // Function to check in a book
  def checkInBook(bookId: Int): Unit = {
    val book = books.find(_.id == bookId)
    book match {
      case Some(b) => {
        b.quantity += 1
        println(s"${b.title} checked in")
      }
      case None => println("Book not found")
    }
  }

  def main(args: Array[String]): Unit = {
    // Example usage of the library functions
    addBook("The Great Gatsby", "F. Scott Fitzgerald", 5)
    addBook("Pride and Prejudice", "Jane Austen", 3)
    addBook("To Kill a Mockingbird", "Harper Lee", 2)

    registerStudent("John Smith")
    registerStudent("Jane Doe")

    showBooks()
    showCustomers(None)

    checkOutBook(1, 1)
    checkOutBook(1, 2)
    checkOutBook(1, 3)

    showBooks()

    checkInBook(1)

    showBooks()
  }
}

