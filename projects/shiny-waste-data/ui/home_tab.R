# First stop for people arriving from the hackathon: explain what this
# prototype does and make the code and contribution path easy to find.
homeTab <- tabPanel(

  "Home",

  h2("What a Waste 3.0"),

  p(
    "This is a hackathon project: jump in, have a play, and follow a question ",
    "that interests you. You do not need to be an expert to explore or contribute. ",
    "AI coding assistants are welcome too; review and explain any changes they make."
  ),

  p(
    "Explore global municipal solid waste data using charts, maps, and ",
    "interactive views. This prototype is a starting point for questions, ",
    "not an official forecast or policy advice."
  ),

  p(
    tags$a(
      "Open the project README for setup, data notes, and contribution guidance",
      href = "https://github.com/Dsposal/What-a-Waste-3.0/tree/main/projects/shiny-waste-data"
    )
  ),

  h3("Pages"),

  tags$ul(
    tags$li("Data Explorer"),
    tags$li("Map"),
    tags$li("Waste Transition"),
    tags$li("Country race"),
    tags$li("EPR and treatment"),
    tags$li("Where waste goes"),
    tags$li("Treatment gap 2050")
  )
)
