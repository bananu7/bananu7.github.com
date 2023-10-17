---
title: Nova, a homegrown kubernetes - part 1
date: 2023-16-10 00:00:00 Z
layout: post
categories:
- kubernetes
---

# Running a (micro)k8s cluster at home - part 1

The first thing to clear up here is _why_ you'd want to do this. Kubernetes is used primarily to
orchestrate large server deployments with abundant resources, not homelab servers. I think I have a compelling case
for actually using it as opposed to merely having a toy running. Manual server
administration gets very tedious _very_ quickly. While it's easy to simply run
a process, making it survive restarts already requires asking the OS for
some orchestration capabilities. Unfortunately, such a setup is often quite
fragile, and very likely to break with OS updates or migrations.

In contrast, your investment in a declarative description of your cluster is
something that will monotonically grow with your needs and knowledge, allowing
you to build on top of things you've already figured out. Experimenting with new
technology is much easier, as you can deploy new things without worrying about
breaking existing deployments or the OS itself via global package management.
And of course, once you have more hardware, it lends itself naturally to
spanning over multiple machines.

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

## Goals and practical uses for my cluster

Running a server often becomes a chicken an egg problem of "why you need a server" and "once I have a server, what do I run on it". Ideally, you'd have at
least one actual use case requiring certain level of availability (more than
simply launching something on your desktop or laptop). For me, I started with
two in mind

* Running the backend server for my RTS game (CPU, memory intensive)
* Storing the rehearsal recordings of my band (storage intensive)

Identifying the needs of each workload will help you choose the right hardware,
k8s distribution and other parts of your deployment.

## First deployment

In the part 2, we'll replay my path as if it was a fresh cluster, so that you can follow along. I've made many more mistakes myself, but there's no reason why you should repeat them. I'll talk a bit more about hardware considerations
and options and cost estimates.
