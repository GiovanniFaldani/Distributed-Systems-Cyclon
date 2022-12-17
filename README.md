### Cyclon implementation for Distributed Systems
Defense mechanism against byzantines:
Remember what you received last update
Receive from Q1, don't update list.
Ask view from Q2, check how similar the views from Q1 and Q2 are
if they're identical up to a certain threshold (say 50%) assume they're both
byzantine and don't update