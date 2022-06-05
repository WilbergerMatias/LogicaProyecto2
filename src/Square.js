import React from 'react';
import { colorToCss } from './Game';

class Square extends React.Component {
    render() {
        return (
            <div 
                style={{ backgroundColor: colorToCss(this.props.value) }} 
                onClick={this.props.onClick}
                className={this.props.className}
            />
        );
    }
}

export default Square;