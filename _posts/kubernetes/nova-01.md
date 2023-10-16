---
title: Nova, a homegrown kubernetes - part 1
date: 2023-16-10 00:00:00 Z
layout: post
categories:
- kubernetes
---

# Running a (micro)k8s cluster at home, part 1

The first thing to clear up here is _why_ you'd want to do this. Kubernetes is used primarily to
orchestrate large server deployments with abundant resources, not homelab servers. I think I have a compelling case
for actually using it as opposed to merely having a toy running.

## My road to orchestration

I wanted to run a small toy server for a while. I've been using cloud options to play
around with, but their pay-per-use pricing wasn't very compelling. I got my hands on a NUC-like
thin client small PC, and decided to deploy it in my living room. It had 4GB of RAM and a dual-core i3 CPU.

By that point I was already leaning heavily on containerisation, using it extensively for
CI builds on GitHub. From a friend, I learned about a new toy for managing containers called Portainer. It seemed like everything I could need - basically a fancy web-based remote
access to the server that would allow me to run various loads, neatly isolated from each other.

I've reached the limits of Portainer very quickly. While fine for manual management in lieu
of running containers by hand, `docker-compose` quickly got limiting and I decided to take
my first leap into Kubernetes.

## First deployment

From now on, we'll replay my path as if it was a fresh cluster