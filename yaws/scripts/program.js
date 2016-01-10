'use strict';

function ProgramStep() {
    this.action = 'Visa';
    this.time = 2;
    this.pictureId = '';
}

ProgramStep.prototype.duplicate = function () {
    var clone = new ProgramStep();
    clone.action = this.action;
    clone.time = this.time;
    clone.pictureId = this.pictureId;
    return clone;
}
