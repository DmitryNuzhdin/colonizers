package colonizers

import com.vaadin.flow.component.Text
import com.vaadin.flow.component.orderedlayout.VerticalLayout
import com.vaadin.flow.router.Route

@Route
class MainView extends VerticalLayout{
  add(new Text("Welcome to SCALA"))
}
