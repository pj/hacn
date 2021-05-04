import React from 'react';
import clsx from 'clsx';
import styles from './HomepageFeatures.module.css';
import Highlight, { defaultProps } from "prism-react-renderer";

import Prism from "prism-react-renderer/prism";

(typeof global !== "undefined" ? global : window).Prism = Prism;

require("prismjs/components/prism-fsharp");

import vsDark from 'prism-react-renderer/themes/vsDark';
import dedent from 'dedent';

const FeatureList = [
  {
    title: 'Reduce complexity',
    Svg: require('../../static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
        A unique approach to control flow turns callbacks in hooks and
        events into a simple linear sequence of operations.
      </>
    ),
  },
  {
    title: 'Handle dependencies',
    Svg: require('../../static/img/undraw_docusaurus_tree.svg').default,
    description: (
      <>
        Dependencies between your API requests, state and props are handled
        automatically or can be controlled manually.
      </>
    ),
  },
  {
    title: 'Integrate with your components',
    Svg: require('../../static/img/undraw_docusaurus_react.svg').default,
    description: (
      <>
        Integrates easily with your existing Fable React components making
        adoption easy.
      </>
    ),
  },
];

function Feature({ Svg, title, description }) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <Svg className={styles.featureSvg} alt={title} />
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
    </div>
  );
}

export function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}

function HomepageCodeBlock(props) {
  return (
    <Highlight
      {...defaultProps}
      code={dedent(props.children)}
      language="fsharp"
      theme={vsDark}
    >
      {({ className, style, tokens, getLineProps, getTokenProps }) => (
        <div className={styles.codeBlock}>
          <pre className={className} style={style}>
            {tokens.map((line, i) => (
              <div {...getLineProps({ line, key: i })}>
                {line.map((token, key) => (
                  <span {...getTokenProps({ token, key })} />
                ))}
              </div>
            ))}
          </pre>
        </div>
      )}
    </Highlight>
  );
}

export function HomepageDetail() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          <div className="col col--8 col--offset-2">
            <p>
              Hacn can be thought of is a DSL for writing React components using
              F#/Fable. It uses computation expressions to provide a unique form
              of control flow with a number of interesting features:
            </p>
            <ul>
              <li>
                Just like Async/await transforms callbacks into a sequence of
                promises, Hacn transforms effect hooks and event handler into a
                series of operations.
              </li>
              <li>
                Operations aren't simply run once but can signal that they have
                an update and then re-execute the steps after the operation.
                This is like a promise resolving multiple times.
              </li>
              <li>
                When an operation signals that it has changed and any operations
                that were running are cancelled. For example a web request that
                depends on props will be cancelled and restarted if the props
                change.
              </li>
            </ul>
            <h2>Examples</h2>
            <h4>Rendering</h4>
            <p>Rendering is performed using an ordinary operation:</p>
            <HomepageCodeBlock>
              {`
              react {
                  do! Render Html.div [
                      prop.class "wrapper"
                      prop.children [
                        Html.div [
                          prop.text "Hello World!"
                        ]
                      ]
                    ]
              }
            `}
            </HomepageCodeBlock>
            <h4>Changed Operations</h4>
            <p>
              Changes to an operation cause the subsequent code to be executed
              again:
            </p>
            <HomepageCodeBlock>
              {`
              react {
                  let! count = Counter 1000
                  do! Render Html.div [
                      prop.text (sprintf "Current count: %d" count)
                    ]
              }
            `}
            </HomepageCodeBlock>
            <h4>Event Capture</h4>
            <p>
              Events are handled by capturing the results of callbacks and
              returning them into the component flow:
            </p>
            <HomepageCodeBlock>
              {`
              react {
                  let! message = Render Html.input [
                      prop.type' "text"
                      prop.captureValueChange
                    ]
                
                  if message = "Hello World!" then
                      do! Render Html.div [
                          prop.text "Hello to you to!"
                        ]
              }
            `}
            </HomepageCodeBlock>
            <h4>Operation Dependencies</h4>
            <p>
              Dependencies between operations are handled automatically -
              changes in earlier operations cause later ones to be cancelled:
            </p>
            <HomepageCodeBlock>
              {`
              react {
                  let! props = Props
                  // Counter is cancelled when props changes
                  let! count = Counter props.interval
                  do! Render Html.div [
                      prop.text (
                        sprintf 
                          "Count: %d, Interval: %d" 
                          count 
                          props.interval
                        )
                    ]
              }
            `}
            </HomepageCodeBlock>
            <h4>Exception Handling</h4>
            <p>
              Component errors can be handled with the normal try/with syntax:
            </p>
            <HomepageCodeBlock>
              {`
              react {
                  try 
                      do! Render ErroringComponent []
                  with
                  | e -> 
                      do! Render Html.div [
                          prop.text e.Exception.Message
                        ]
              }
            `}
            </HomepageCodeBlock>
          </div>
          {/* {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))} */}
        </div>
      </div>
    </section>
  );
}