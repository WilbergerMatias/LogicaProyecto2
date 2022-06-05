import React from 'react';
import PopUp from "./PopUp";

class Popup extends React.Component {


    render() {
        return (
            <div className="modal">
                <div className="modal_content">
                    {this.props.texto}
                </div>
            </div>
        );
    }

}

export default Popup;