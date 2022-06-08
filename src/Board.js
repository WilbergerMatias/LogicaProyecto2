import React from 'react';
import Square from './Square';


class Board extends React.Component {
    render() {
        return (
            <div className="board">
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square
                            value={cell}
                            key={i + "." + j}
                            onClick={this.props.onOriginSelected && (() =>
                                this.props.onOriginSelected(i,j))}
                            className={this.props.origin[0] === i && this.props.origin[1] === j ? "origin" : undefined}
                        />
                    )
                )}
            </div>
        );
    }
}

export default Board;