import { LitElement, html, css } from 'lit'

class ProgressRing extends LitElement {
  #normalizedRadius
  #circumference
  #strokeDasharray
  #strokeDashoffset
  static get styles () {
    return css`
      :host {
        /* positioning the progress and the title
        --progress-from-bottom: 10%;
        --progress-from-left: 50%;
        --title-from-top: 40%;
        --title-from-left: 50%;
        /* background color of the circle 
        --bk-color: transparent; */
        /* progress indicator color / stop gradient-color / opacity 
        --progress-stroke-color: purple;
        --progress-stroke-gradient-color: #00bc9b;
        --progress-stroke-linecap: round;
        --progress-opacity: .9; */
        /* trace of the indicator path and opacity
        --footprint-stroke-color: grey;
        --footprint-opacity: .2; */
        /* progress text style 
        --progress-font-family: 'Roboto', sans-serif;
        --progress-font-size: 2rem;
        --progress-font-weight: 700;
        --progress-font-color: salmon; */
        /* title text style 
        --title-font-family: 'Roboto', sans-serif;
        --title-font-size: 1.3rem;
        --title-font-color: blue; */

        display: inline-block;
      }

      .content {
        position: relative;
        text-align: center;
      }

      .foodprint-ring__circle {
        opacity: var(--footprint-opacity, 0);
        stroke: var(--footprint-stroke-color, transparent);
      }

      .progress-ring__circle {
        fill: var(--bk-color, transparent);
        stroke-linecap: var(--progress-stroke-linecap, butt);
        /* stroke: var(--progress-stroke-color); */
        opacity: var(--progress-opacity, .9);
        /* One particular thing about stroke-dashoffset, its starting point 
          is vertically centered and horizontally tilted to the right. It’s 
          necessary to negatively rotate the circle to get the desired effect. */
        transform: rotate(-90deg);
        transform-origin: 50% 50%;
        /* by transitioning the property we will get the animation feel */
        transition: stroke-dashoffset 0.35s;
      }

      #gradient {
        color: var(--progress-stroke-color, #e9008d);
      }

      #stop {
        color: var(--progress-stroke-gradient-color, var(--progress-stroke-color));
      }

      .title {
        font-family: var(--title-font-family, sans-serif);
        font-size: var(--title-font-size, 1.2rem);
        color: var(--title-font-color, #000);
        position: absolute;
        top: var(--title-from-top, 40%);
        left: var(--title-from-left, 50%);
        transform: translate(-50%, -50%);
      }

      .progress {
        font-family: var(--progress-font-family, sans-serif);
        font-size: var(--progress-font-size, 2rem);
        font-weight: var(--progress-font-weight, 400);
        color: var(--progress-font-color, #000);
        position: absolute;
        bottom: var(--progress-from-bottom, 10%);
        left: var(--progress-from-left, 50%);
        transform: translate(-45%, -45%);
      }
    `
  }

  static get properties () {
    return {
      title: String,
      radius: Number,
      progress: Number,
      stroke: Number
    }
  }

  constructor () {
    super()
    this.title = ''
    this.radius = 60
    this.progress = 0
    this.stroke = 10
  }

  connectedCallback () {
    super.connectedCallback()
    this.#normalizedRadius = this.radius - this.stroke
    this.#circumference = this.#normalizedRadius * 2 * Math.PI
    this.#strokeDasharray = `${this.#circumference} ${this.#circumference}`
    this.#strokeDashoffset = 
        this.#circumference - this.progress / 100 * this.#circumference
  }

  update (changed) {
    // handle the progress updated
    if (changed.has('progress')) {
      this.#strokeDashoffset = 
        this.#circumference - this.progress / 100 * this.#circumference
    }
    super.update()
  }

  render () {
    return html`
    <div class="content">

      <svg class="progress-ring" 
        height=${this.radius * 2} 
        width=${this.radius * 2} >

        <defs>
          <linearGradient id="gradient" x1="0%" y1="0%" x2="0%" y2="100%">
            <stop offset="0%"  stop-color="currentcolor" />
            <stop id="stop" offset="100%" stop-color="currentcolor" />
          </linearGradient>
        </defs>

        <circle 
          class="foodprint-ring__circle"
          shape-rendering="geometricPrecision"
          stroke-width=${this.stroke}
          fill="transparent"
          r=${this.#normalizedRadius}
          cx=${this.radius}
          cy=${this.radius} />

        <circle 
          class="progress-ring__circle"
          shape-rendering="geometricPrecision"
          stroke="url(#gradient)"
          stroke-width=${this.stroke}
          r=${this.#normalizedRadius}
          cx=${this.radius}
          cy=${this.radius}
          stroke-dasharray=${this.#strokeDasharray}
          stroke-dashoffset=${this.#strokeDashoffset} />

      </svg>

      <p class="title">${this.title}</p>
      <p class="progress">${this.progress}%</p>

  </div>
    `
  }
}

customElements.define('progress-ring', ProgressRing)
