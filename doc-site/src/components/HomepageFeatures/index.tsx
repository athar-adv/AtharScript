import type {ReactNode} from 'react';
import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

type FeatureItem = {
  title: string;
  Svg: React.ComponentType<React.ComponentProps<'svg'>>;
  description: ReactNode;
};

const FeatureList: FeatureItem[] = [
  {
    title: 'Fast',
    Svg: require('@site/static/img/fast.svg').default,
    description: (
      <>
        Lucy's VM is built in Rust and takes advantage of that to run quite fast!
      </>
    ),
  },
  {
    title: 'Simple',
    Svg: require('@site/static/img/simple.svg').default,
    description: (
      <>
        Lucy is designed to be simple and not have much non-normalized syntax nor semantics, similar to Luau, which means
        it should be very easy to learn and use!
        It also is going to get some very nice tools such as package managers later on in the development
      </>
    ),
  },
  {
    title: 'Open Source',
    Svg: require('@site/static/img/open.svg').default,
    description: (
      <>
        There is extensive documentation on how this language was designed,
        from the lexer, ast, compiler to the vm.
        This makes it much easier for beginners to learn how to make similar projects!
      </>
    ),
  },
];

function Feature({title, Svg, description}: FeatureItem) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures(): ReactNode {
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
