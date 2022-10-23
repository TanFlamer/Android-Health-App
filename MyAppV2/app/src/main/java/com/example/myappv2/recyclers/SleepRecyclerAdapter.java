package com.example.myappv2.recyclers;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myappv2.R;

import java.util.List;

public class SleepRecyclerAdapter extends RecyclerView.Adapter<SleepRecyclerHolder> {

    Context context;
    List<SleepRecyclerItem> items;

    public SleepRecyclerAdapter(Context context, List<SleepRecyclerItem> items){
        this.context = context;
        this.items = items;
    }

    @NonNull
    @Override
    public SleepRecyclerHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        return new SleepRecyclerHolder(LayoutInflater.from(context).inflate(R.layout.sleep_recycler_item, parent,false));
    }

    @Override
    public void onBindViewHolder(@NonNull SleepRecyclerHolder holder, int position) {
        holder.dateView.setText(items.get(position).getDate());
        holder.sleepView.setText(items.get(position).getSleepTime());
        holder.wakeView.setText(items.get(position).getWakeTime());
        holder.durationView.setText(String.valueOf(items.get(position).getSleepDuration()));
    }

    @Override
    public int getItemCount() {
        return items.size();
    }
}
