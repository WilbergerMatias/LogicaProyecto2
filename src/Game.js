import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import PopUp from "./PopUp";
import swal from 'sweetalert';


/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  switch (color) {
    case "r": return "red";
    case "v": return "violet";
    case "p": return "pink";
    case "g": return "green";
    case "b": return "blue";
    case "y": return "yellow";
  }
  return color;
}
class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      turns: 0,
      PosX: 0,
      PosY: 0,
      grid: null,
      iniciado : false,
        capturadas: 0,
      movimiento: [],
      complete: false,  // true if game is complete, false otherwise
        waiting: false,
      seen: false
    };
    this.handleClick = this.handleClick.bind(this);
    this.onOriginSelected = this.onOriginSelected.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
      swal("Bienvenido a flick-color", "Al comienzo del juego, puede seleccionar cualqueira de los cuadrados de la grilla a la derecha, para empezar en ese cuadrado, este estara con un borde oscuro para resaltar. Por defecto este es el de el tope a la izquierda. Una vez que selecciona un lugar de partida, puede usar los 6 botones de la izquierda para cambiar el color del principal y de todos los del mismo color conectados.", "info");
  }

  togglePop = () => {
        this.setState({
            seen: !this.state.seen
        });
    };

  handlePengineCreate() {
    const queryS = 'init(Grid)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid']
        });
      }
    });
  }

  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    if (this.state.complete || this.state.waiting) {
      return;
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
    //        [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
    //        [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
    //        [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
    //        [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
    //        [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
    //        [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
    //        [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
    //        [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
    //        [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
    //        [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
    //        [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
    //        [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],r, Grid)
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
      const queryS = "flick(" + gridS + "," + color + "," + this.state.PosX +","+ this.state.PosY+", Grid)";
    this.setState({
      waiting: true,
      iniciado: true
    });
    this.pengine.query(queryS, (success, response) => {
        if (success) {
        this.setState({
          grid: response['Grid'],
            turns: this.state.turns + 1,
        });
          //const principal = this.state.grid[this.state.PosX][this.state.PosY];
            const grid2 = JSON.stringify(this.state.grid).replaceAll('"', "");
            const query2 = "adyacentes(" + grid2 + "," + color + "," + this.state.PosX + "," + this.state.PosY + ", [], Rta)";
            this.pengine.query(query2, (success, response) => {
                if (success) {
                    const Aux = response['Rta'];
                    this.setState({
                        capturadas: Aux.length,
                    });
                    if (Aux.length === this.state.grid.length * this.state.grid[0].length)
                        this.setState({
                            complete: true
                        })
                }
                if (this.state.complete!==1)
                    this.setState({
                        waiting: false,
                    });
            });
      } else {
        // Prolog query will fail when the clicked color coincides with that in the top left cell.
        this.setState({
          waiting: false
        });
        }
        this.state.movimiento.unshift(colorToCss(color)) // moviendo esta linea de lugar, se puede hacer que se actualice solamente si es valido el movimiento
    });
  }

  onOriginSelected(x, y) {
    this.setState({
      PosX : x,
      PosY : y,
      iniciado : true
    });
  }

  render() {
    if (this.state.grid === null) {
      return null;
    }
    return (
      <div className="game">
        <div className="leftPanel">
          <div className="buttonsPanel">
            {colors.map(color =>
              <button
                className="colorBtn"
                style={{ backgroundColor: colorToCss(color) }}
                onClick={() => this.handleClick(color)}
                key={color}
              />)}
          </div>
          <div className="subPanel">
            <div className="turnsLab">Turnos</div>
                    <div className="turnsNum">{this.state.turns}</div>
            <div className="capturadasLab">Capturadas</div>
                    <div className="capturadasNum"> {this.state.capturadas}</div>
            <div className="historyLab">movimientos</div>
                  <div className="historyTab">
                        {this.state.movimiento.map((color, mov) =>
                        <button
                            className="colorBtn"
                            style={{ backgroundColor: colorToCss(color) }}
                            key={mov}
                        />)}
                  </div>       
          </div>  
          </div>
            <Board 
              grid={this.state.grid} 
              onOriginSelected = {!this.state.iniciado && !this.state.posX && !this.state.PosY ? this.onOriginSelected : undefined}
              origin= {[this.state.PosX, this.state.PosY]}
            />
            {this.state.complete ? <PopUp texto={"Victoria!!! Logro completar el juego en un total de: " + this.state.turns + " flicks realizados."} /> : null}
      </div>
    );
  }
}

export default Game;