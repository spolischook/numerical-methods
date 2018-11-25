library(shiny)
library(DT)
library(markdown)
library(readtext)
source('../../../includes/lagrange_polynomial.R', chdir = T)
source('../../../includes/makeXYmatrix.R', chdir = T)
source('../../../includes/method_iteration.R', chdir = T)
source('../../../includes/method_dichotomy.R', chdir = T)
source('../../../includes/method_newton.R', chdir = T)

f1 <- function(x) {
  return (x ^ 2 - cos(x))
}
df1 <- function(x) {
  return (2*x + sin(x))
}
f2 <- function(v) {
  return (function(x) {
    return (v['A']*x^12 + v['B']*x^10 + v['C']*x^8 + v['D']*x^6 + v['E']*x^4 + v['F']*x^2 + v['G'])
  })
}
f3 <- function(x) {
  return (1 / (1 + x))
}

task4GraphicPercise = 10

ui <- navbarPage(
  "Курсовий Проект",
  tabPanel(
    "Завдання 1",
    titlePanel("Розв'язування нелінійних рівнянь з однією змінною"),
    
    
    sidebarLayout(
      sidebarPanel(
        includeMarkdown("tasks/task1.md"),
        withMathJax(),
        splitLayout(
          tags$h3("\\(x^{2}-cos(x) = \\)"),
          numericInput('y', '', 0, step=0.1)
        ),
        sliderInput(
          "task1borders",
          "Межі функції:",
          min = -4,
          max = 4,
          step = 0.1,
          value = c(-2, 2)
        ),
        sliderInput(
          "precision",
          "Точність:",
          min = 1,
          max = 10,
          step = 1,
          value = 3
        ),
        selectInput(
          "task1method",
          "Метод розв'язку:",
          c(
            "Ітерацій"  = "iteration",
            "Дихотомії" = "dichotomy",
            "Н'ютона"   = "newton"
          )
        ),
        tags$h4('Корені рівняння: '),
        DT::dataTableOutput("task1roots")
      ),
      
      mainPanel(
        plotOutput("task1plot"),
        tags$h4('Пошук коренів: '),
        DT::dataTableOutput("task1calc")
      )
    )
  ),
  tabPanel(
    "Завдання 2",
    titlePanel("Резонансні частоти для заданих коефіцієнтів"),
    withMathJax(),
    tags$h3(
      "\\(A\\omega^{12} +B\\omega^{10} +C\\omega^{8} +D\\omega^{6} +E\\omega^{4} +F\\omega^{2} +G = 0\\)"
    ),
    sidebarLayout(
      sidebarPanel(
        includeMarkdown("tasks/task2.md"),
        sliderInput(
          "variant",
          "Варіант: ",
          min = 1,
          max = 6,
          step = 1,
          value = 1
        ),
        sliderInput(
          "task2borders",
          "Межі функції:",
          min = -10,
          max = 10,
          step = 0.1,
          value = c(-4, 4)
        ),
        selectInput(
          "task2method",
          "Метод розв'язку:",
          c(
            "Ітерацій"  = "iteration",
            "Дихотомії" = "dichotomy"
          )
        ),
        tags$h4('Корені рівняння: '),
        DT::dataTableOutput("task2roots")
      ),
      mainPanel(
        plotOutput("task2plot"),
        DT::dataTableOutput("task2variants"),
        tags$h4('Пошук коренів: '),
        DT::dataTableOutput("task2calc")
      )
    )
  ),
  tabPanel(
    "Завдання 3",
    titlePanel("Методи знаходження інтегралів"),
    withMathJax(),
    tags$h3("Формула: \\(\\int_{a}^{b} \\frac{1}{1+x}\\)"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "integralBorders",
          "Межі інтегрування:",
          min = -1.1,
          max = -0.9,
          step = 0.01,
          value = c(-1.1, -0.9)
        )
      ),
      
      mainPanel(
        plotOutput("task3plot"),
        DT::dataTableOutput("task2datatable")
      )
    )
  ),
  tabPanel(
    "Завдання 4",
    titlePanel("Інтерполяційний многочлен Лагранжа"),
    withMathJax(),
    tags$h3("Формула: \\(\\frac{1}{1+x}\\)"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "task4borders",
          "Межі інтегрування:",
          min = -2,
          max = 2,
          step = 0.05,
          value = c(0, 1)
        ),
        DT::dataTableOutput("task4datatable")
      ),
      
      mainPanel(plotOutput("task4plot"),
        uiOutput('task4solution'),
        withMathJax(),
        tags$h3(
          "\\(L(x) = \\sum_{j=0}^{n}y_{j}l_{j}(x)\\)"
        ),
        tags$h3(
          "\\(l_{j}(x) = \\prod_{i=0; j\\neq i}^{n} \\frac{x -x_{i}}{x_{j} - x_{i}}\\)"
        )
      )
    )
  )
  
)

server <- function(input, output, session) {
  output$task1plot <- renderPlot({
    Xs <- input$task1borders[1]
    Xe <- input$task1borders[2]
    y  <- input$y
    x  <- seq(Xs, Xe, by = 0.001)
    precision <- input$precision
    
    par(mai = c(1, 1, 0, 1))
    plot(x, f1(x), type = 'l')
    abline(h = y)

    switch(input$task1method,
      iteration={
        iN = abs(Xs - Xe)/10^-precision
        if (iN > 10000) stop(simpleError("Too much iterations"))
        result <- iteration(f1, Xs, Xe, y, 10^-precision)
      },
      dichotomy={
        result <- dichotomy(f1, Xs, Xe, y, 10^-precision)
      },
      newton={
        result <- nm.newton(f1, df1, Xs, Xe, y, 10^-precision)
      }
    )

    output$task1calc <- DT::renderDataTable({
      DT::datatable(result[[1]])
    })
    output$task1roots <- DT::renderDataTable({
      DT::datatable(
        result[[2]],
        list(
          'paging' = FALSE,
          'searching' = FALSE
        )
      )
    })
    for (i in seq_along(result[[2]][ , 'x'])) {
      points(
        result[[2]][i, 'x'],
        result[[2]][i, 'y'],
        col='red'
      )
    }
  })

  # Task2
  m = matrix(
    data = c(
       0.01,      0,           0,            0.1,         1,           0,
       1,         0.01,        0.02,        -20,         -29900,       1,
      -78,        1,           0.1,          102,        -29900,       1,
       2.1*10^3, -1.25*10^3,  -2.56*10^3,   -8.98*10^3,   0,          -116,
      -2.5*10^4,  1.85*10^5,   3.45*10^5,    8.76*10^6,   26400,       4.3*10^3,
       1.2*10^5, -8.75*10^6,  -9.95*10^6,   -7.5*10^5,    9.12*10^8,  -5.3*10.4,
      -1.9*10^5,  8.9*10^7,    2.7*10^7,    -3.3*10^8,    -1.75*10^9,  8.9*10^4
    ),
    nrow = 7,
    ncol = 6,
    byrow = TRUE,
    dimnames = list(
      LETTERS[seq( from = 1, to = 7 )],
      seq(1,6)
    )
  )
  output$task2variants <- DT::renderDataTable(
    m,
    options = list(
      'paging' = FALSE,
      'searching' = FALSE
    )
  )
  output$task2plot <- renderPlot({
    Xs <- input$task2borders[1]
    Xe <- input$task2borders[2]
    y = 0
    precision=3
    f <- f2(m[ ,input$variant])

    x <- seq(Xs, Xe, by=0.01)
    par(mai = c(1, 1, 0, 1))
    plot(x, f(x), type='l', ylab='f(x)')
    abline(h = y)

    switch(input$task2method,
      iteration={
        result <- iteration(f, Xs, Xe, y, 10^-precision)
      },
      dichotomy={
        result <- dichotomy(f, Xs, Xe, y, 10^-precision)
      }
    )

    output$task2calc <- DT::renderDataTable({
      DT::datatable(result[[1]])
    })
    output$task2roots <- DT::renderDataTable({
      DT::datatable(
        result[[2]],
        list(
          'paging' = FALSE,
          'searching' = FALSE
        )
      )
    })
    for (i in seq_along(result[[2]][ , 'x'])) {
      points(
        result[[2]][i, 'x'],
        result[[2]][i, 'y'],
        col='red'
      )
    }
  })
  
  # Task4
  output$task4plot <- renderPlot({
    left  <- input$task4borders[1]
    right <- input$task4borders[2]
    X <- seq(left, right, by = 0.001)
    Y <- f3(X)
    maxY <- max(Y) + 1
    minY <- min(Y) - 1
    
    x <- seq(left, right, by = (right - left) / task4GraphicPercise)
    
    plot(X,
         Y,
         type = 'l',
         ylim = c(if (is.infinite(minY) |
                      minY < -60)
           - 60
           else
             minY,
           if (is.infinite(maxY) | maxY > 60)
             60
           else
             maxY))
    points(x,
           f3(x),
           col = rgb(0, 0, 1, 0.5))
    
    abline(h = 0)
  })
  output$task4datatable <- DT::renderDataTable({
    left  <- input$task4borders[1]
    right <- input$task4borders[2]
    x <- seq(left, right, by = (right - left) / task4GraphicPercise)
    m <- makeXYmatrix(x, f3)
    
    lagrange <- lagrangePolynomial(x, m)
    m <- cbind(m, lagrange)
    
    DT::datatable(
      m,
      options = list(
        pageLength = 11,
        'paging' = FALSE,
        'searching' = FALSE
      )
    )
  })
}

shinyApp(ui = ui, server = server)
