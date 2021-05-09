/** @type {import('@docusaurus/types').DocusaurusConfig} */
module.exports = {
  title: 'Hacn',
  tagline: 'Experimental DSL for creating React components in F#/Fable',
  url: 'https://hacn.dev',
  baseUrl: '/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  organizationName: 'hacn', // Usually your GitHub org/user name.
  projectName: 'hacn', // Usually your repo name.
  themeConfig: {
    navbar: {
      title: 'Hacn',
      logo: {
        alt: 'Hacn Logo',
        src: 'img/logo.svg',
      },
      items: [
        {
          type: 'doc',
          docId: 'getting-started/tutorial',
          position: 'left',
          label: 'Tutorial',
        },
        {
          href: 'https://github.com/hacn/hacn',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          items: [
            {
              label: 'Tutorial',
              to: '/docs',
            },
            {
              label: 'Stack Overflow',
              href: 'https://stackoverflow.com/questions/tagged/hacn',
            },
            {
              label: 'GitHub',
              href: 'https://github.com/facebook/docusaurus',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Hacn. Built with Docusaurus.`,
    },
  },
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          editUrl:
            'https://github.com/facebook/docusaurus/edit/master/website/',
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          editUrl:
            'https://github.com/facebook/docusaurus/edit/master/website/blog/',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
};
