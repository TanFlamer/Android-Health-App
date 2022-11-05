package com.example.myapp.fragmentsSleep.recyclerSleep;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SleepRecyclerAdapter extends RecyclerView.Adapter<SleepRecyclerAdapter.SleepRecyclerItemViewHolder> {

    Context context;
    List<SleepRecyclerItem> sleepRecyclerItemList;

    public SleepRecyclerAdapter(Context context, List<SleepRecyclerItem> sleepRecyclerItemList){
        this.context = context;
        this.sleepRecyclerItemList = sleepRecyclerItemList;
    }

    @NonNull
    @Override
    public SleepRecyclerItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.sleep_recycler_list_item, parent, false);
        return new SleepRecyclerItemViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull SleepRecyclerItemViewHolder holder, int position) {
        SleepRecyclerItem sleepRecyclerItem = sleepRecyclerItemList.get(position);

        holder.titleView.setText(sleepRecyclerItem.getTitle());
        holder.dateView.setText(sleepRecyclerItem.getDate());
        holder.sleepView.setText(sleepRecyclerItem.getSleepTime());
        holder.wakeView.setText(sleepRecyclerItem.getWakeTime());
        holder.durationView.setText(String.valueOf(sleepRecyclerItem.getSleepDuration()));

        boolean isShown = sleepRecyclerItemList.get(position).isShown();
        holder.layoutHidden.setVisibility(isShown ? View.VISIBLE : View.GONE);
    }

    @Override
    public int getItemCount() {
        return sleepRecyclerItemList.size();
    }

    public class SleepRecyclerItemViewHolder extends RecyclerView.ViewHolder {

        Map<LinearLayout, Boolean> linearLayoutBooleanMap = new HashMap<>();
        TextView titleView, dateView, sleepView, wakeView, durationView;
        LinearLayout layoutVisible, layoutHidden;

        public SleepRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);

            titleView = itemView.findViewById(R.id.sleepTitle);
            dateView = itemView.findViewById(R.id.sleepDate);
            sleepView = itemView.findViewById(R.id.sleepTime);
            wakeView = itemView.findViewById(R.id.wakeTime);
            durationView = itemView.findViewById(R.id.sleepDuration);

            layoutVisible = itemView.findViewById(R.id.sleepLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sleepLayoutHidden);

            layoutVisible.setOnClickListener(view -> {
                SleepRecyclerItem sleepRecyclerItem = sleepRecyclerItemList.get(getAdapterPosition());
                sleepRecyclerItem.setShown(!sleepRecyclerItem.isShown());
                notifyItemChanged(getAdapterPosition());
            });

            hideLayout(itemView.findViewById(R.id.sleepDateVisible), itemView.findViewById(R.id.sleepDateHidden));
            hideLayout(itemView.findViewById(R.id.sleepTimeVisible), itemView.findViewById(R.id.sleepTimeHidden));
            hideLayout(itemView.findViewById(R.id.wakeTimeVisible), itemView.findViewById(R.id.wakeTimeHidden));
            hideLayout(itemView.findViewById(R.id.sleepDurationVisible), itemView.findViewById(R.id.sleepDurationHidden));
        }

        public void hideLayout(LinearLayout layoutVisible, LinearLayout layoutHidden){
            layoutHidden.setVisibility(View.GONE);
            linearLayoutBooleanMap.put(layoutHidden, false);
            layoutVisible.setOnClickListener(view -> {
                linearLayoutBooleanMap.put(layoutHidden, Boolean.FALSE.equals(linearLayoutBooleanMap.get(layoutHidden)));
                layoutHidden.setVisibility(Boolean.TRUE.equals(linearLayoutBooleanMap.get(layoutHidden)) ? View.VISIBLE : View.GONE);
            });
        }
    }
}
